//! Post-pass extraction of debug info from the emitted object file.
//!
//! The runtime backtrace walker needs exact source positions and argument
//! values per frame, but DWARF sections are not allocated into the running
//! binary (the runtime cannot read them). So the compiler parses the object
//! it just emitted and folds the interesting parts into compact, alloc'd,
//! link-time-resolved tables (`__silver_bt_lines`, `__silver_bt_args`).
//!
//! This module implements the ELF + DWARF subset needed:
//! - `.symtab`: function symbols (name, .text offset, size).
//! - `.debug_line` (DWARF v4 and v5): (address, line, file) pairs.
//! - `.debug_info`/`.debug_abbrev`/`.debug_str`: per-subprogram formal
//!   parameter names and frame offsets (for reading spilled arguments off the
//!   frame at abort time). AArch64 describes some slots relative to SP, which
//!   the walker cannot see; each function's prologue gives the SP-to-x29
//!   distance that turns them into frame-pointer offsets.
//!
//! Addresses in the object are `.text`-relative; absolute relocations
//! (`R_X86_64_64`/`_32`, `R_AARCH64_ABS64`/`ABS32`) against the debug
//! sections are applied (they carry the section symbol's value plus the
//! addend).

use rustc_hash::FxHashMap as HashMap;
use rustc_hash::FxHashSet as HashSet;

/// One line entry: byte offset from the function's start (stable across
/// linking — the linker moves whole functions, never their insides).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BtLineEntry {
    pub offset: u64,
    pub line: u64,
    /// Source file basename.
    pub file: String,
}

/// Where a formal parameter lives in the frame.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BtParamLoc {
    /// `DW_OP_fbreg N`: `N` bytes from the frame pointer (`rbp` or `x29`),
    /// which `"frame-pointer"="all"` makes the frame base.
    Fbreg(i64),
    /// `DW_OP_breg31 N` on AArch64: `N` bytes above SP once the prologue has
    /// run. LLVM uses it for slots nearer SP than the frame pointer.
    SpOffset(i64),
    /// `DW_OP_regN`: register-resident, cannot be read from the frame.
    Reg(u32),
    /// Anything else (loclists, complex expressions).
    Unknown,
}

/// Debug info for one emitted function.
#[derive(Debug, Clone)]
pub struct BtFnDebug {
    pub name: String,
    /// Sorted by offset; one entry per line transition.
    pub lines: Vec<BtLineEntry>,
    pub params: Vec<(String, BtParamLoc)>,
    /// AArch64: how far the frame pointer sits above SP after the prologue;
    /// `None` on other targets or when the prologue is not recognised.
    pub fp_above_sp: Option<i64>,
}

// ---------------------------------------------------------------------------
// Reader
// ---------------------------------------------------------------------------

struct Reader<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Reader<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }
    fn u8(&mut self) -> Option<u8> {
        let b = *self.data.get(self.pos)?;
        self.pos += 1;
        Some(b)
    }
    fn u16(&mut self) -> Option<u16> {
        let b = self.data.get(self.pos..self.pos + 2)?;
        self.pos += 2;
        Some(u16::from_le_bytes([b[0], b[1]]))
    }
    fn u32(&mut self) -> Option<u32> {
        let b = self.data.get(self.pos..self.pos + 4)?;
        self.pos += 4;
        Some(u32::from_le_bytes([b[0], b[1], b[2], b[3]]))
    }
    fn u64(&mut self) -> Option<u64> {
        let b = self.data.get(self.pos..self.pos + 8)?;
        self.pos += 8;
        Some(u64::from_le_bytes([
            b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7],
        ]))
    }
    fn uleb(&mut self) -> Option<u64> {
        let mut result: u64 = 0;
        let mut shift = 0;
        loop {
            let byte = self.u8()?;
            if shift >= 64 {
                return None;
            }
            result |= u64::from(byte & 0x7f) << shift;
            if byte & 0x80 == 0 {
                return Some(result);
            }
            shift += 7;
        }
    }
    fn sleb(&mut self) -> Option<i64> {
        let mut result: i64 = 0;
        let mut shift = 0;
        loop {
            let byte = self.u8()?;
            if shift >= 64 {
                return None;
            }
            result |= i64::from(byte & 0x7f) << shift;
            shift += 7;
            if byte & 0x80 == 0 {
                if shift < 64 && byte & 0x40 != 0 {
                    result |= -1i64 << shift;
                }
                return Some(result);
            }
        }
    }
    fn skip(&mut self, n: usize) {
        self.pos = (self.pos + n).min(self.data.len());
    }
    fn cstr(&mut self) -> Option<String> {
        let start = self.pos;
        while let Some(b) = self.data.get(self.pos) {
            if *b == 0 {
                let s = std::str::from_utf8(&self.data[start..self.pos]).ok()?;
                self.pos += 1;
                return Some(s.to_string());
            }
            self.pos += 1;
        }
        None
    }
    fn bytes(&mut self, n: usize) -> Option<&'a [u8]> {
        let b = self.data.get(self.pos..self.pos + n)?;
        self.pos += n;
        Some(b)
    }
}

// ---------------------------------------------------------------------------
// ELF
// ---------------------------------------------------------------------------

const SHT_SYMTAB: u32 = 2;
const SHT_RELA: u32 = 4;

struct ElfSection {
    name: String,
    name_off: u32,
    offset: usize,
    size: usize,
    link: u32,
    info: u32,
    entsize: u64,
    ty: u32,
}

struct ElfSym {
    name: String,
    value: u64,
    size: u64,
    is_func: bool,
    defined: bool,
    /// COFF only: 1-based section number (0 = undefined).
    section: i16,
}

struct ObjectFile<'a> {
    data: &'a [u8],
    /// ELF `e_machine`; COFF objects are x86-64.
    machine: u16,
    sections: Vec<ElfSection>,
    symbols: Vec<ElfSym>,
    /// debug-section-relative offset -> value to add (symbol value + addend)
    relocations: HashMap<usize, u64>,
}

const EM_X86_64: u16 = 62;
const EM_AARCH64: u16 = 183;

/// Whether ELF relocation `r_type` on `machine` stores `symbol + addend`, the
/// only kind the debug sections of our objects carry.
fn absolute_relocation(machine: u16, r_type: u32) -> bool {
    match machine {
        // R_X86_64_64, R_X86_64_32
        EM_X86_64 => matches!(r_type, 1 | 10),
        // R_AARCH64_ABS64, R_AARCH64_ABS32
        EM_AARCH64 => matches!(r_type, 257 | 258),
        _ => false,
    }
}

fn elf_parse(data: &[u8]) -> Option<ObjectFile<'_>> {
    if !data.starts_with(b"\x7fELF\x02\x01\x01") {
        return None;
    }
    let mut r = Reader::new(data);
    r.skip(16); // e_ident
    let _ = r.u16()?; // e_type
    let e_machine = r.u16()?;
    let _ = r.u32()?; // e_version
    let _ = r.u64()?; // e_entry
    let _ = r.u64()?; // e_phoff
    let e_shoff = r.u64()? as usize;
    let _ = r.u32()?; // e_flags
    let _ = r.u16()?; // e_ehsize
    let _ = r.u16()?; // e_phentsize
    let _ = r.u16()?; // e_phnum
    let e_shentsize = r.u16()? as usize;
    let e_shnum = r.u16()? as usize;
    let e_shstrndx = r.u16()? as usize;
    if e_shentsize < 64 {
        return None;
    }
    let mut sections = Vec::new();
    for i in 0..e_shnum {
        let mut sr = Reader::new(data.get(e_shoff + i * e_shentsize..)?);
        let sh_name = sr.u32()?;
        let sh_type = sr.u32()?;
        let _ = sr.u64()?; // flags
        let _ = sr.u64()?; // addr
        let sh_offset = sr.u64()? as usize;
        let sh_size = sr.u64()? as usize;
        let sh_link = sr.u32()?;
        let sh_info = sr.u32()?;
        let _ = sr.u64()?; // addralign
        let sh_entsize = sr.u64()?;
        sections.push(ElfSection {
            name: String::new(),
            name_off: sh_name,
            offset: sh_offset,
            size: sh_size,
            link: sh_link,
            info: sh_info,
            entsize: sh_entsize,
            ty: sh_type,
        });
    }
    let shstr_off = sections.get(e_shstrndx)?.offset;
    for sec in &mut sections {
        let mut sr = Reader::new(data.get(shstr_off..)?);
        sr.skip(sec.name_off as usize);
        sec.name = sr.cstr().unwrap_or_default();
    }
    let mut symbols = Vec::new();
    for sec in &sections {
        if sec.ty != SHT_SYMTAB {
            continue;
        }
        let strtab = sections.get(sec.link as usize)?;
        let entsize = if sec.entsize == 0 {
            24
        } else {
            sec.entsize as usize
        };
        for i in 0..sec.size / entsize {
            let mut sr = Reader::new(data.get(sec.offset + i * entsize..)?);
            let st_name = sr.u32()? as usize;
            let st_info = sr.u8()?;
            let _ = sr.u8()?;
            let st_shndx = sr.u16()?;
            let st_value = sr.u64()?;
            let st_size = sr.u64()?;
            let mut nsr = Reader::new(data.get(strtab.offset..)?);
            nsr.skip(st_name);
            let name = nsr.cstr().unwrap_or_default();
            symbols.push(ElfSym {
                name,
                value: st_value,
                size: st_size,
                is_func: st_info & 0xf == 2,
                defined: st_shndx != 0,
                section: st_shndx as i16,
            });
        }
    }
    let mut relocations = HashMap::default();
    for sec in &sections {
        if sec.ty != SHT_RELA {
            continue;
        }
        // sh_info names the section the relocations apply to (sh_link is
        // the symbol table).
        let Some(target) = sections.get(sec.info as usize) else {
            continue;
        };
        for i in 0..sec.size / 24 {
            let Some(ent) = data.get(sec.offset + i * 24..sec.offset + i * 24 + 24) else {
                continue;
            };
            let mut rr = Reader::new(ent);
            let r_offset = rr.u64()? as usize;
            let r_info = rr.u64()?;
            let r_addend = rr.u64()? as i64 as u64;
            let sym_idx = (r_info >> 32) as usize;
            let r_type = (r_info & 0xffff_ffff) as u32;
            if absolute_relocation(e_machine, r_type) {
                // value = sym + addend
                let val = symbols.get(sym_idx).map(|s| s.value).unwrap_or(0);
                relocations.insert(target.offset + r_offset, val.wrapping_add(r_addend));
            }
        }
    }
    Some(ObjectFile {
        data,
        machine: e_machine,
        sections,
        symbols,
        relocations,
    })
}

// ---------------------------------------------------------------------------
// COFF (x64) container — Windows objects. Mirrors the ObjectFile contract:
// named sections, defined function symbols (section-relative values, sizes
// from the function aux records), and a debug-section-relative relocation
// map for ADDR64/ADDR32/SECREL against symbol values.
// ---------------------------------------------------------------------------

const IMAGE_FILE_MACHINE_AMD64: u16 = 0x8664;
const IMAGE_SYM_TYPE_FUNCTION: u16 = 0x20;
const COFF_REL_ADDR64: u16 = 1;
const COFF_REL_ADDR32: u16 = 2;
const COFF_REL_SECREL: u16 = 11;

fn coff_parse(data: &[u8]) -> Option<ObjectFile<'_>> {
    let mut r = Reader::new(data);
    let machine = r.u16()?;
    if machine != IMAGE_FILE_MACHINE_AMD64 {
        return None;
    }
    let number_of_sections = r.u16()? as usize;
    let _time_stamp = r.u32()?;
    let sym_table_off = r.u32()? as usize;
    let sym_count = r.u32()? as usize;
    let _opt_size = r.u16()?;
    let sym_bytes = sym_count.checked_mul(18)?;
    if number_of_sections > 96
        || sym_table_off == 0
        || sym_count == 0
        || sym_table_off.checked_add(sym_bytes)? > data.len()
    {
        return None;
    }

    // Section headers immediately follow the 20-byte file header. Long names
    // (all .debug_* names exceed 8 bytes) are "/<offset>" into the string
    // table, resolved after it is located.
    struct Sec {
        name: String,
        raw_ptr: usize,
        raw_size: usize,
        reloc_ptr: usize,
        reloc_count: usize,
        long_name_off: Option<usize>,
    }
    let mut secs: Vec<Sec> = Vec::with_capacity(number_of_sections);
    for i in 0..number_of_sections {
        let mut sr = Reader::new(data.get(20 + i * 40..)?);
        let mut name_bytes = [0u8; 8];
        for b in name_bytes.iter_mut() {
            *b = sr.u8()?;
        }
        let _virtual_size = sr.u32()?;
        let _virtual_address = sr.u32()?;
        let raw_size = sr.u32()? as usize;
        let raw_ptr = sr.u32()? as usize;
        let reloc_ptr = sr.u32()? as usize;
        let _lineno_ptr = sr.u32()?;
        let reloc_count = sr.u16()? as usize;
        let _lineno_count = sr.u16()?;
        let _characteristics = sr.u32()?;

        let (name, long_name_off) = if name_bytes[0] == b'/' {
            let digits = &name_bytes[1..];
            let end = digits.iter().position(|&c| c == 0).unwrap_or(digits.len());
            let off: usize = std::str::from_utf8(&digits[..end]).ok()?.parse().ok()?;
            (String::new(), Some(off))
        } else {
            let end = name_bytes.iter().position(|&c| c == 0).unwrap_or(8);
            (
                String::from_utf8_lossy(&name_bytes[..end]).into_owned(),
                None,
            )
        };
        secs.push(Sec {
            name,
            raw_ptr,
            raw_size,
            reloc_ptr,
            reloc_count,
            long_name_off,
        });
    }

    // String table sits immediately after the symbol table; its first four
    // bytes hold the total size (including the size field itself).
    let strtab_off = sym_table_off + sym_count * 18;
    let strtab_size = Reader::new(data.get(strtab_off..)?).u32()? as usize;
    let strtab = data.get(strtab_off..strtab_off + strtab_size)?;

    for sec in &mut secs {
        if let Some(off) = sec.long_name_off {
            let mut sr = Reader::new(strtab.get(off..)?);
            sec.name = sr.cstr().unwrap_or_default();
        }
    }

    // Symbol table: 18-byte records; aux records occupy entries too, so the
    // Vec stays index-aligned with COFF symbol indices.
    let mut symbols: Vec<ElfSym> = Vec::with_capacity(sym_count);
    let mut i = 0usize;
    while i < sym_count {
        let rec = data.get(sym_table_off + i * 18..sym_table_off + (i + 1) * 18)?;
        let mut sr = Reader::new(rec);
        let mut name_bytes = [0u8; 8];
        for b in name_bytes.iter_mut() {
            *b = sr.u8()?;
        }
        let value = sr.u32()? as u64;
        let section_number = sr.u16()? as i16;
        let sym_type = sr.u16()?;
        let _storage_class = sr.u8()?;
        let aux_count = sr.u8()? as usize;

        let name = if name_bytes[0] == b'/' {
            let digits = &name_bytes[1..];
            let end = digits.iter().position(|&c| c == 0).unwrap_or(digits.len());
            match std::str::from_utf8(&digits[..end])
                .ok()
                .and_then(|s| s.parse::<usize>().ok())
            {
                Some(off) => {
                    let mut nsr = Reader::new(strtab.get(off..)?);
                    nsr.cstr().unwrap_or_default()
                }
                None => String::new(),
            }
        } else if name_bytes[0..4] == [0, 0, 0, 0] {
            // Modern long-name format: four zero bytes, then a u32 LE
            // string-table offset. Names longer than eight bytes (all mangled
            // functions) use it; decoding them as empty would merge distinct
            // backtrace entries under one name.
            let off =
                u32::from_le_bytes([name_bytes[4], name_bytes[5], name_bytes[6], name_bytes[7]])
                    as usize;
            let mut nsr = Reader::new(strtab.get(off..)?);
            nsr.cstr().unwrap_or_default()
        } else {
            let end = name_bytes.iter().position(|&c| c == 0).unwrap_or(8);
            String::from_utf8_lossy(&name_bytes[..end]).into_owned()
        };

        // Function size comes from the function aux record's TotalSize field.
        let mut size = 0u64;
        if aux_count > 0 {
            if let Some(aux) = data.get(sym_table_off + (i + 1) * 18..sym_table_off + (i + 2) * 18)
            {
                let mut ar = Reader::new(aux);
                ar.u32(); // tag index
                size = ar.u32().unwrap_or(0) as u64; // total size
            }
        }

        symbols.push(ElfSym {
            name,
            value,
            size,
            is_func: (sym_type & IMAGE_SYM_TYPE_FUNCTION) != 0,
            defined: section_number > 0,
            section: section_number,
        });
        for _ in 0..aux_count {
            symbols.push(ElfSym {
                name: String::new(),
                value: 0,
                size: 0,
                is_func: false,
                defined: false,
                section: 0,
            });
        }
        i += 1 + aux_count;
    }

    // COFF function symbols carry no size (LLVM emits no aux records), so
    // derive each function's extent from the next function symbol in the
    // same section — push_entry bounds line entries by [start, start+size).
    {
        let mut per_section: HashMap<i16, Vec<usize>> = HashMap::default();
        for (idx, sym) in symbols.iter().enumerate() {
            if sym.is_func && sym.defined {
                per_section.entry(sym.section).or_default().push(idx);
            }
        }
        let mut ordered: Vec<(usize, u64, u64)> = Vec::new(); // (sym idx, value, section end)
        for (sec_no, mut idxs) in per_section {
            let Some(sec_idx) = (sec_no as usize).checked_sub(1) else {
                continue;
            };
            let Some(sec) = secs.get(sec_idx) else {
                continue;
            };
            idxs.sort_by_key(|&i| symbols[i].value);
            for w in 0..idxs.len() {
                let next = if w + 1 < idxs.len() {
                    symbols[idxs[w + 1]].value
                } else {
                    sec.raw_size as u64
                };
                ordered.push((idxs[w], symbols[idxs[w]].value, next));
            }
        }
        for (idx, _val, end) in ordered {
            if end > symbols[idx].value {
                symbols[idx].size = end - symbols[idx].value;
            }
        }
    }

    // Relocations: per-section records (VirtualAddress u32, SymbolTableIndex
    // u32, Type u16), applied in place — debug info expects the symbol's
    // section-relative value, matching the ELF object semantics.
    let mut relocations = HashMap::default();
    for sec in &secs {
        for j in 0..sec.reloc_count {
            let Some(ent) = data.get(sec.reloc_ptr + j * 10..sec.reloc_ptr + j * 10 + 10) else {
                continue;
            };
            let mut rr = Reader::new(ent);
            let vaddr = rr.u32()? as usize;
            let sym_idx = rr.u32()? as usize;
            let rel_type = rr.u16()?;
            if rel_type == COFF_REL_ADDR64
                || rel_type == COFF_REL_ADDR32
                || rel_type == COFF_REL_SECREL
            {
                let val = symbols.get(sym_idx).map(|s| s.value).unwrap_or(0);
                relocations.insert(sec.raw_ptr + vaddr, val);
            }
        }
    }

    Some(ObjectFile {
        data,
        machine: EM_X86_64,
        sections: secs
            .into_iter()
            .map(|s| ElfSection {
                name: s.name,
                name_off: 0,
                offset: s.raw_ptr,
                size: s.raw_size,
                link: 0,
                info: 0,
                entsize: 0,
                ty: 0,
            })
            .collect(),
        symbols,
        relocations,
    })
}

/// Parse either container: ELF (Linux) or COFF (Windows).
fn object_parse(data: &[u8]) -> Option<ObjectFile<'_>> {
    elf_parse(data).or_else(|| coff_parse(data))
}

fn elf_section<'a>(elf: &ObjectFile<'a>, name: &str) -> Option<&'a [u8]> {
    for sec in &elf.sections {
        if sec.name == name {
            return elf.data.get(sec.offset..sec.offset + sec.size);
        }
    }
    None
}

// ---------------------------------------------------------------------------
// .debug_line
// ---------------------------------------------------------------------------

fn basename(path: &str) -> String {
    path.rsplit(['/', '\\']).next().unwrap_or(path).to_string()
}

/// Returns the set of defined function symbol names with code in the object.
pub fn parse_defined_function_symbols(obj: &[u8]) -> HashSet<String> {
    let Some(elf) = object_parse(obj) else {
        return HashSet::default();
    };
    elf.symbols
        .iter()
        .filter(|s| s.is_func && s.defined && s.size > 0)
        .map(|s| s.name.clone())
        .collect()
}

/// Parse all line tables; returns per-function line entries.
pub fn parse_object_debug_lines(obj: &[u8]) -> Vec<BtFnDebug> {
    let Some(elf) = object_parse(obj) else {
        return Vec::new();
    };
    let Some(line_data) = elf_section(&elf, ".debug_line") else {
        return Vec::new();
    };
    // Relocation keys are file-absolute (target section offset + r_offset);
    // resolve the line table's own offset once so per-row lookups can use
    // section-relative positions.
    let line_sec_off = elf
        .sections
        .iter()
        .find(|s| s.name == ".debug_line")
        .map(|s| s.offset)
        .unwrap_or(0);
    let funcs: Vec<(String, u64, u64)> = elf
        .symbols
        .iter()
        .filter(|s| s.is_func && s.defined && s.size > 0)
        .map(|s| (s.name.clone(), s.value, s.size))
        .collect();
    if funcs.is_empty() {
        return Vec::new();
    }
    let fp_above_sp: HashMap<String, i64> = if elf.machine == EM_AARCH64 {
        elf.symbols
            .iter()
            .filter(|s| s.is_func && s.defined && s.size > 0)
            .filter_map(|s| {
                let section = elf.sections.get(usize::from(s.section as u16))?;
                let start = section.offset.checked_add(usize::try_from(s.value).ok()?)?;
                let code = elf.data.get(start..start.checked_add(usize::try_from(s.size).ok()?)?)?;
                Some((s.name.clone(), aarch64_fp_above_sp(code)?))
            })
            .collect()
    } else {
        HashMap::default()
    };
    let mut per_fn: HashMap<String, Vec<(u64, u64, String)>> = HashMap::default();

    let mut pos = 0usize;
    while pos + 4 <= line_data.len() {
        let mut ur = Reader::new(&line_data[pos..]);
        let Some(unit_len) = ur.u32() else { break };
        if unit_len == 0 || unit_len as usize > line_data.len() - pos - 4 {
            break;
        }
        let table = &line_data[pos + 4..pos + 4 + unit_len as usize];
        let mut r = Reader::new(table);
        let Some(version) = r.u16() else { break };
        let is_v5 = version >= 5;
        if is_v5 {
            r.u8(); // address size
            r.u8(); // segment selector size
        }
        let header_len = r.u32().unwrap_or(0) as usize;
        let prog_start_in_table = r.pos + header_len;
        let prog = table.get(prog_start_in_table..).unwrap_or(&[]);
        let mut hr = Reader::new(&table[r.pos..(r.pos + header_len).min(table.len())]);

        let min_inst_len = hr.u8().unwrap_or(1) as u64;
        let max_ops = hr.u8().unwrap_or(1) as u64;
        hr.u8(); // default_is_stmt
        let line_base = hr.u8().unwrap_or(0xfb) as i8 as i64;
        let line_range = hr.u8().unwrap_or(14) as u64;
        let opcode_base = hr.u8().unwrap_or(13);
        let std_op_len: Vec<u8> = hr
            .bytes(opcode_base.saturating_sub(1) as usize)
            .unwrap_or(&[])
            .to_vec();

        // Directories. Peek the terminator instead of consuming the first
        // name byte.
        let mut dirs: Vec<String> = Vec::new();
        while hr.data.get(hr.pos).is_some_and(|&c| c != 0) {
            if is_v5 {
                hr.uleb(); // dir index
            }
            let name = hr.cstr().unwrap_or_default();
            if is_v5 {
                hr.uleb(); // mtime
                hr.uleb(); // size
            }
            dirs.push(name);
        }
        hr.pos += 1; // terminator
        // Files: name, dir index, mtime, size (v4 and v5 both carry the
        // three ulebs after the name).
        let mut files: Vec<(String, usize)> = Vec::new();
        while hr.data.get(hr.pos).is_some_and(|&c| c != 0) {
            let name = hr.cstr().unwrap_or_default();
            let dir_idx = hr.uleb().unwrap_or(0) as usize;
            hr.uleb(); // mtime
            hr.uleb(); // size
            files.push((name, dir_idx));
        }
        hr.pos += 1; // terminator
        let _ = dirs;

        // Program state machine.
        let mut pr = Reader::new(prog);
        let mut address: u64 = 0;
        let mut line: i64 = 1;
        let mut file_idx: usize = 1;
        while let Some(opcode) = pr.u8() {
            if opcode == 0 {
                // Extended opcode: length-prefixed.
                let len = pr.uleb().unwrap_or(0) as usize;
                let sub_start = pr.pos;
                let sub = pr.bytes(len).unwrap_or(&[]);
                let mut sr = Reader::new(sub);
                match sr.u8().unwrap_or(0) {
                    0x01 => {
                        // end sequence
                        address = 0;
                        line = 1;
                        file_idx = 1;
                    }
                    0x02 => {
                        // set address (64-bit, may be relocated)
                        let raw = sr.u64().unwrap_or(0);
                        let field_off =
                            line_sec_off + pos + 4 + prog_start_in_table + sub_start + 1;
                        let reloc = elf.relocations.get(&field_off).copied().unwrap_or(0);
                        address = raw.wrapping_add(reloc);
                    }
                    0x03 => {
                        // define file
                        let _ = sr.cstr();
                        sr.uleb();
                        sr.uleb();
                        sr.uleb();
                    }
                    0x04 => {
                        line = sr.sleb().unwrap_or(line);
                    }
                    0x05 => {
                        let _ = sr.uleb();
                    }
                    0x09 => {
                        let _ = sr.u16();
                    }
                    _ => {}
                }
            } else if opcode < opcode_base {
                // Standard opcodes: apply the ones that move the state.
                match opcode {
                    1 => {
                        // DW_LNS_copy
                        push_entry(&mut per_fn, &funcs, &files, file_idx, address, line);
                    }
                    2 => {
                        // DW_LNS_advance_pc
                        let adv = pr.uleb().unwrap_or(0);
                        address += adv * min_inst_len;
                    }
                    3 => {
                        // DW_LNS_advance_line
                        line += pr.sleb().unwrap_or(0);
                    }
                    4 => {
                        // DW_LNS_set_file
                        file_idx = pr.uleb().unwrap_or(0) as usize;
                    }
                    5 => {
                        let _ = pr.uleb();
                    }
                    8 => {
                        // DW_LNS_const_add_pc
                        address +=
                            ((255 - u64::from(opcode_base)) / line_range.max(1)) * min_inst_len;
                    }
                    9 => {
                        let _ = pr.u16();
                    }
                    12 => {
                        let _ = pr.uleb();
                    }
                    _ => {
                        let n = std_op_len.get(opcode as usize - 1).copied().unwrap_or(0) as usize;
                        for _ in 0..n {
                            pr.uleb();
                        }
                    }
                }
            } else {
                // Special opcode.
                let adjusted = u64::from(opcode - opcode_base);
                address += (adjusted / line_range.max(1)) * min_inst_len;
                line += line_base + (adjusted % line_range.max(1)) as i64;
                push_entry(&mut per_fn, &funcs, &files, file_idx, address, line);
            }
        }
        let _ = max_ops;
        pos += 4 + unit_len as usize;
    }

    let mut out = Vec::new();
    for (name, mut lines) in per_fn {
        lines.sort_by_key(|l| l.0);
        let mut collapsed: Vec<(u64, u64, String)> = Vec::new();
        for entry in lines {
            if let Some(last) = collapsed.last_mut()
                && last.1 == entry.1
                && last.2 == entry.2
            {
                continue;
            }
            collapsed.push(entry);
        }
        out.push(BtFnDebug {
            fp_above_sp: fp_above_sp.get(&name).copied(),
            name,
            lines: collapsed
                .into_iter()
                .map(|(offset, line, file)| BtLineEntry { offset, line, file })
                .collect(),
            params: Vec::new(),
        });
    }
    out
}

/// How far x29 sits above SP once the AArch64 prologue at the start of
/// `code` has run. LLVM saves the callee-saved registers, sets x29 with
/// `add x29, sp, #K` (`mov x29, sp` is K = 0), then may lower SP further with
/// `sub sp, sp, #N`. Any other SP write after x29 is set (stack realignment,
/// a register-sized frame) makes SP-relative slots unreadable from x29, so
/// the result is `None` rather than a wrong offset.
fn aarch64_fp_above_sp(code: &[u8]) -> Option<i64> {
    // Prologue instructions before x29 is set: PAC/BTI hints, one SP bump,
    // and every callee-saved register pair (x19-x28, d8-d15).
    const MAX_BEFORE_FP: usize = 16;
    // Body instructions checked for a late SP write such as `and sp, x9, #m`.
    const BODY_WINDOW: usize = 4;
    let words: Vec<u32> = code
        .chunks_exact(4)
        .map(|w| u32::from_le_bytes([w[0], w[1], w[2], w[3]]))
        .collect();
    let add_imm = |word: u32| i64::from((word >> 10) & 0xfff) << if word & (1 << 22) != 0 { 12 } else { 0 };
    let fp_setup = words
        .iter()
        .take(MAX_BEFORE_FP)
        .position(|&w| w & 0xff80_03ff == 0x9100_03fd)?; // add x29, sp, #imm
    let mut above = add_imm(words[fp_setup]);
    let mut rest = words[fp_setup + 1..].iter().copied().peekable();
    while let Some(word) = rest.next_if(|&w| w & 0xff80_03ff == 0xd100_03ff) {
        above += add_imm(word); // sub sp, sp, #imm
    }
    // Rd = 31 names SP only in the forms that do not set flags.
    let writes_sp = |w: u32| {
        let add_sub_imm = w & 0x3f80_0000 == 0x1100_0000;
        let add_sub_extended = w & 0x3fe0_0000 == 0x0b20_0000;
        let logical_imm = w & 0x1f80_0000 == 0x1200_0000 && w & 0x6000_0000 != 0x6000_0000;
        w & 0x1f == 0x1f && (add_sub_imm || add_sub_extended || logical_imm)
    };
    if rest.take(BODY_WINDOW).any(writes_sp) {
        return None;
    }
    Some(above)
}

fn push_entry(
    per_fn: &mut HashMap<String, Vec<(u64, u64, String)>>,
    funcs: &[(String, u64, u64)],
    files: &[(String, usize)],
    file_idx: usize,
    address: u64,
    line: i64,
) {
    if file_idx == 0 || file_idx > files.len() {
        return;
    }
    let (fname, _) = &files[file_idx - 1];
    let fname = basename(fname);
    for (f, start, size) in funcs {
        if address >= *start && address < start + size {
            per_fn.entry(f.clone()).or_default().push((
                address - start,
                line as u64,
                fname.clone(),
            ));
            break;
        }
    }
}

// ---------------------------------------------------------------------------
// .debug_info (formal parameters)
// ---------------------------------------------------------------------------

const DW_AT_NAME: u64 = 0x03;
const DW_AT_LOCATION: u64 = 0x02;
const DW_AT_LINKAGE_NAME: u64 = 0x6e;
const DW_AT_ARTIFICIAL: u64 = 0x34;
const DW_AT_MIPS_LINKAGE_NAME: u64 = 0x2007;

const DW_TAG_SUBPROGRAM: u64 = 0x2e;
const DW_TAG_FORMAL_PARAMETER: u64 = 0x05;

const DW_OP_FBREG: u8 = 0x91;
const DW_OP_BREG31: u8 = 0x8f;
const DW_OP_REG0: u8 = 0x50;
const DW_OP_REGX: u8 = 0x90;

const DW_FORM_ADDR: u64 = 0x01;
const DW_FORM_BLOCK2: u64 = 0x03;
const DW_FORM_BLOCK4: u64 = 0x04;
const DW_FORM_DATA2: u64 = 0x05;
const DW_FORM_DATA4: u64 = 0x06;
const DW_FORM_DATA8: u64 = 0x07;
const DW_FORM_STRING: u64 = 0x08;
const DW_FORM_BLOCK: u64 = 0x09;
const DW_FORM_BLOCK1: u64 = 0x0a;
const DW_FORM_DATA1: u64 = 0x0b;
const DW_FORM_FLAG: u64 = 0x0c;
const DW_FORM_SDATA: u64 = 0x0d;
const DW_FORM_STRP: u64 = 0x0e;
const DW_FORM_UDATA: u64 = 0x0f;
const DW_FORM_REF_ADDR: u64 = 0x10;
const DW_FORM_REF1: u64 = 0x11;
const DW_FORM_REF2: u64 = 0x12;
const DW_FORM_REF4: u64 = 0x13;
const DW_FORM_REF8: u64 = 0x14;
const DW_FORM_REF_UDATA: u64 = 0x15;
const DW_FORM_INDIRECT: u64 = 0x16;
const DW_FORM_SEC_OFFSET: u64 = 0x17;
const DW_FORM_EXPRLOC: u64 = 0x18;
const DW_FORM_FLAG_PRESENT: u64 = 0x19;
const DW_FORM_STRX: u64 = 0x1a;
const DW_FORM_ADDRX: u64 = 0x1b;
const DW_FORM_DATA16: u64 = 0x1e;
const DW_FORM_LINE_STRP: u64 = 0x1f;
const DW_FORM_IMPLICIT_CONST: u64 = 0x21;
const DW_FORM_STRX1: u64 = 0x25;
const DW_FORM_STRX2: u64 = 0x26;
const DW_FORM_STRX3: u64 = 0x27;
const DW_FORM_STRX4: u64 = 0x28;
const DW_FORM_ADDRX1: u64 = 0x29;
const DW_FORM_ADDRX2: u64 = 0x2a;
const DW_FORM_ADDRX3: u64 = 0x2b;
const DW_FORM_ADDRX4: u64 = 0x2c;

/// Parse `.debug_info` formal parameters for the given function names.
pub fn parse_object_params(
    obj: &[u8],
    targets: &HashSet<String>,
) -> Vec<(String, Vec<(String, BtParamLoc)>)> {
    let Some(elf) = object_parse(obj) else {
        return Vec::new();
    };
    let Some(info) = elf_section(&elf, ".debug_info") else {
        return Vec::new();
    };
    let Some(abbrev) = elf_section(&elf, ".debug_abbrev") else {
        return Vec::new();
    };
    let Some(strs) = elf_section(&elf, ".debug_str") else {
        return Vec::new();
    };
    let info_sec_off = elf
        .sections
        .iter()
        .find(|s| s.name == ".debug_info")
        .map(|s| s.offset)
        .unwrap_or(0);
    let mut result: Vec<(String, Vec<(String, BtParamLoc)>)> = Vec::new();

    let mut pos = 0usize;
    while pos + 4 <= info.len() {
        let mut ur = Reader::new(&info[pos..]);
        let Some(unit_len) = ur.u32() else { break };
        if unit_len == 0 || unit_len as usize > info.len() - pos - 4 {
            break;
        }
        let unit_end = pos + 4 + unit_len as usize;
        let cu = &info[pos + 4..unit_end];
        let mut r = Reader::new(cu);
        let version = r.u16().unwrap_or(0);
        let abbrev_off = r.u32().unwrap_or(0) as usize;
        if version >= 5 {
            r.u8(); // address size
        }
        let header_len = if version >= 5 { 8 } else { 7 };
        let mut dr = Reader::new(cu);
        dr.skip(header_len);

        // Abbrev table for this CU.
        let mut abbrev_map: HashMap<u64, (u64, Vec<(u64, u64)>)> = HashMap::default();
        {
            let mut ar = Reader::new(abbrev.get(abbrev_off..).unwrap_or(&[]));
            while let Some(code) = ar.uleb() {
                if code == 0 {
                    break;
                }
                let tag = ar.uleb().unwrap_or(0);
                let _has_children = ar.u8().unwrap_or(0);
                let mut attrs = Vec::new();
                loop {
                    let a = ar.uleb().unwrap_or(0);
                    let f = ar.uleb().unwrap_or(0);
                    if a == 0 && f == 0 {
                        break;
                    }
                    attrs.push((a, f));
                }
                abbrev_map.insert(code, (tag, attrs));
            }
        }

        // Walk the DIE tree: subprograms at depth 1, formal parameters as
        // their direct children. Null DIEs terminate sibling lists.
        let mut current_sub: Option<(String, Vec<(String, BtParamLoc)>)> = None;
        let mut depth = 0i32;
        while let Some(code) = dr.uleb() {
            if code == 0 {
                // End of siblings.
                if let Some((name, params)) = current_sub.take()
                    && targets.contains(&name)
                {
                    result.push((name, params));
                }
                depth -= 1;
                if depth < 0 {
                    break;
                }
                continue;
            }
            let Some(&(tag, ref attrs)) = abbrev_map.get(&code) else {
                break;
            };
            depth += 1;
            let mut name: Option<String> = None;
            let mut linkage: Option<String> = None;
            let mut is_artificial = false;
            let mut loc: Option<BtParamLoc> = None;

            for &(attr, form) in attrs {
                match attr {
                    DW_AT_NAME => {
                        name = read_str(&mut dr, form, strs, pos, info_sec_off, &elf);
                    }
                    DW_AT_LINKAGE_NAME | DW_AT_MIPS_LINKAGE_NAME => {
                        linkage = read_str(&mut dr, form, strs, pos, info_sec_off, &elf);
                    }
                    DW_AT_LOCATION => {
                        loc = read_location(&mut dr, form);
                    }
                    DW_AT_ARTIFICIAL => {
                        is_artificial = read_flag(&mut dr, form).unwrap_or(false);
                    }
                    _ => {
                        skip_form(&mut dr, form);
                    }
                }
            }

            match tag {
                DW_TAG_SUBPROGRAM => {
                    if let Some((n, p)) = current_sub.take()
                        && targets.contains(&n)
                    {
                        result.push((n, p));
                    }
                    let nm = linkage
                        .as_deref()
                        .map(str::to_string)
                        .or_else(|| name.clone())
                        .unwrap_or_default();
                    current_sub = Some((nm, Vec::new()));
                }
                DW_TAG_FORMAL_PARAMETER => {
                    if let Some((_, params)) = current_sub.as_mut()
                        && !is_artificial
                    {
                        let nm = name.clone().unwrap_or_else(|| "<arg>".to_string());
                        params.push((nm, loc.unwrap_or(BtParamLoc::Unknown)));
                    }
                }
                _ => {}
            }
        }
        let _ = version;
        pos = unit_end;
    }
    result
}

fn read_str(
    r: &mut Reader,
    form: u64,
    strs: &[u8],
    cu_pos: usize,
    info_sec_off: usize,
    elf: &ObjectFile,
) -> Option<String> {
    match form {
        DW_FORM_STRING => r.cstr(),
        DW_FORM_STRP => {
            let field_off = info_sec_off + cu_pos + 4 + r.pos;
            let reloc = elf.relocations.get(&field_off).copied().unwrap_or(0);
            let raw = r.u32()? as usize;
            let off = raw.wrapping_add(reloc as usize);
            let mut sr = Reader::new(strs.get(off..).unwrap_or(&[]));
            sr.cstr()
        }
        DW_FORM_STRX | DW_FORM_STRX1 | DW_FORM_STRX2 | DW_FORM_STRX3 | DW_FORM_STRX4 => {
            let _ = r.uleb();
            None
        }
        _ => None,
    }
}

fn read_flag(r: &mut Reader, form: u64) -> Option<bool> {
    match form {
        DW_FORM_FLAG => r.u8().map(|b| b != 0),
        DW_FORM_FLAG_PRESENT => Some(true),
        _ => None,
    }
}

/// DW_AT_location: exprloc with DW_OP_fbreg N / DW_OP_breg31 N / DW_OP_regN /
/// DW_OP_regx.
fn read_location(r: &mut Reader, form: u64) -> Option<BtParamLoc> {
    let bytes = match form {
        DW_FORM_EXPRLOC | DW_FORM_BLOCK => {
            let len = r.uleb()? as usize;
            r.bytes(len)?
        }
        DW_FORM_BLOCK1 => {
            let len = r.u8()? as usize;
            r.bytes(len)?
        }
        DW_FORM_BLOCK2 => {
            let len = r.u16()? as usize;
            r.bytes(len)?
        }
        DW_FORM_BLOCK4 => {
            let len = r.u32()? as usize;
            r.bytes(len)?
        }
        _ => return Some(BtParamLoc::Unknown),
    };
    let mut br = Reader::new(bytes);
    match br.u8()? {
        DW_OP_FBREG => Some(BtParamLoc::Fbreg(br.sleb()?)),
        DW_OP_BREG31 => Some(BtParamLoc::SpOffset(br.sleb()?)),
        op if (DW_OP_REG0..=DW_OP_REG0 + 31).contains(&op) => {
            Some(BtParamLoc::Reg(u32::from(op - DW_OP_REG0)))
        }
        DW_OP_REGX => Some(BtParamLoc::Reg(br.uleb()? as u32)),
        _ => Some(BtParamLoc::Unknown),
    }
}

fn skip_form(r: &mut Reader, form: u64) {
    match form {
        DW_FORM_ADDR | DW_FORM_DATA8 | DW_FORM_REF8 | DW_FORM_REF_ADDR | DW_FORM_DATA16 => {
            r.skip(8);
        }
        DW_FORM_DATA4 | DW_FORM_STRP | DW_FORM_SEC_OFFSET | DW_FORM_LINE_STRP | DW_FORM_REF4 => {
            r.skip(4);
        }
        DW_FORM_DATA2 | DW_FORM_REF2 | DW_FORM_BLOCK2 => {
            r.skip(2);
        }
        DW_FORM_DATA1 | DW_FORM_FLAG | DW_FORM_REF1 | DW_FORM_BLOCK1 => {
            r.skip(1);
        }
        DW_FORM_FLAG_PRESENT => {
            // Implicit true — no data in the stream.
        }
        DW_FORM_STRING => {
            r.cstr();
        }
        DW_FORM_SDATA | DW_FORM_IMPLICIT_CONST => {
            r.sleb();
        }
        DW_FORM_UDATA | DW_FORM_REF_UDATA | DW_FORM_STRX | DW_FORM_STRX1 | DW_FORM_STRX2
        | DW_FORM_STRX3 | DW_FORM_STRX4 | DW_FORM_ADDRX | DW_FORM_ADDRX1 | DW_FORM_ADDRX2
        | DW_FORM_ADDRX3 | DW_FORM_ADDRX4 | DW_FORM_INDIRECT => {
            r.uleb();
        }
        DW_FORM_EXPRLOC | DW_FORM_BLOCK => {
            let len = r.uleb().unwrap_or(0) as usize;
            r.skip(len);
        }
        DW_FORM_BLOCK4 => {
            let len = r.u32().unwrap_or(0) as usize;
            r.skip(len);
        }
        _ => {
            r.skip(8);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::aarch64_fp_above_sp;

    fn code(words: &[u32]) -> Vec<u8> {
        words.iter().flat_map(|w| w.to_le_bytes()).collect()
    }

    #[test]
    fn aarch64_prologue_distance() {
        // sub sp, sp, #0x20; stp x29, x30, [sp, #0x10]; add x29, sp, #0x10; str x0, [sp]
        let combined = code(&[0xd100_83ff, 0xa901_7bfd, 0x9100_43fd, 0xf900_03e0]);
        assert_eq!(aarch64_fp_above_sp(&combined), Some(0x10));
        // stp x29, x30, [sp, #-0x10]!; mov x29, sp; sub sp, sp, #0x1, lsl #12; sub sp, sp, #0x30
        let separate = code(&[0xa9bf_7bfd, 0x9100_03fd, 0xd140_07ff, 0xd100_c3ff, 0xf900_03e0]);
        assert_eq!(aarch64_fp_above_sp(&separate), Some(0x1030));
        // stp x29, x30, [sp, #-0x10]!; mov x29, sp; sub x9, sp, #0x40; and sp, x9, #~0x1f
        let realigned = code(&[0xa9bf_7bfd, 0x9100_03fd, 0xd101_03e9, 0x927b_e93f]);
        assert_eq!(aarch64_fp_above_sp(&realigned), None);
        // No frame pointer setup.
        assert_eq!(aarch64_fp_above_sp(&code(&[0xd100_83ff, 0xd65f_03c0])), None);
    }
}
