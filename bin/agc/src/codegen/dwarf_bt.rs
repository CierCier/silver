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
//!   parameter names and `DW_OP_fbreg` frame offsets (for reading spilled
//!   arguments off the frame at abort time).
//!
//! Addresses in the object are `.text`-relative; `R_X86_64_64` relocations
//! against `.debug_line` are applied (they carry the section symbol's value
//! plus the addend, which is 0 for our own objects).

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
    /// `DW_OP_fbreg N`: read `N + CFA` bytes; CFA = rbp + 16 with
    /// `"frame-pointer"="all"`.
    Fbreg(i64),
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
}

struct ElfFile<'a> {
    data: &'a [u8],
    sections: Vec<ElfSection>,
    symbols: Vec<ElfSym>,
    /// debug-section-relative offset -> value to add (symbol value + addend)
    relocations: HashMap<usize, u64>,
}

fn elf_parse(data: &[u8]) -> Option<ElfFile<'_>> {
    if !data.starts_with(b"\x7fELF\x02\x01\x01") {
        return None;
    }
    let mut r = Reader::new(data);
    r.skip(16); // e_ident
    let _ = r.u16()?; // e_type
    let _ = r.u16()?; // e_machine
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
            if r_type == 1 || r_type == 10 {
                // R_X86_64_64 (1) and R_X86_64_32 (10): value = sym + addend
                let val = symbols.get(sym_idx).map(|s| s.value).unwrap_or(0);
                relocations.insert(target.offset + r_offset, val.wrapping_add(r_addend));
            }
        }
    }
    Some(ElfFile {
        data,
        sections,
        symbols,
        relocations,
    })
}

fn elf_section<'a>(elf: &ElfFile<'a>, name: &str) -> Option<&'a [u8]> {
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
    let Some(elf) = elf_parse(obj) else {
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
    let Some(elf) = elf_parse(obj) else {
        return Vec::new();
    };
    let Some(line_data) = elf_section(&elf, ".debug_line") else {
        return Vec::new();
    };
    let funcs: Vec<(String, u64, u64)> = elf
        .symbols
        .iter()
        .filter(|s| s.is_func && s.defined && s.size > 0)
        .map(|s| (s.name.clone(), s.value, s.size))
        .collect();
    if funcs.is_empty() {
        return Vec::new();
    }
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
                        let field_off = pos + 4 + prog_start_in_table + sub_start + 1;
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
    let Some(elf) = elf_parse(obj) else {
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
    elf: &ElfFile,
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

/// DW_AT_location: exprloc with DW_OP_fbreg N / DW_OP_regN / DW_OP_regx.
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
    use super::*;

    #[test]
    fn parses_line_table_from_real_object() {
        let dir = std::env::temp_dir();
        let src = dir.join("dwarf_bt_probe.ag");
        std::fs::write(
            &src,
            "import std.io;\n\
             i64 f3(i64 x) { return x * 2; }\n\
             i64 f2(i64 x) { return f3(x + 1); }\n\
             void main() { i64 r = f2(41); @println(\"{}\", r); }\n",
        )
        .unwrap();
        let out = dir.join("dwarf_bt_probe.o");
        let _ = std::fs::remove_file(&out);
        let agc = std::env::var("AGC").unwrap_or_else(|_| {
            let target = std::env::var("CARGO_TARGET_DIR")
                .unwrap_or_else(|_| concat!(env!("CARGO_MANIFEST_DIR"), "/../../target").to_string());
            // Match the test binary's profile: `cargo test --release` never
            // rebuilds target/debug/agc, so a stale cached debug binary would
            // compile the probe against a drifted frontend.
            let profile = if cfg!(debug_assertions) { "debug" } else { "release" };
            format!("{target}/{profile}/agc")
        });
        let status = std::process::Command::new(&agc)
            .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/../.."))
            .args([
                "-c",
                "-g",
                src.to_str().unwrap(),
                "-o",
                out.to_str().unwrap(),
            ])
            .status()
            .expect("run agc");
        assert!(status.success(), "agc -c -g failed: {status}");
        let obj = std::fs::read(&out).expect("read object");
        let funcs = parse_object_debug_lines(&obj);
        let f2 = funcs.iter().find(|f| f.name == "f2").expect("f2 in table");
        assert!(!f2.lines.is_empty(), "f2 has line entries");
        assert_eq!(f2.lines[0].line, 3, "first entry is f2's decl line");
        assert_eq!(f2.lines[0].file, "dwarf_bt_probe.ag", "file is the probe");
        let main = funcs
            .iter()
            .find(|f| f.name == "main")
            .expect("main in table");
        assert!(
            main.lines.iter().any(|l| l.line >= 4),
            "main has body lines"
        );

        // Params: f2's parameter `x` must resolve to a frame slot.
        let targets: HashSet<String> = ["f2", "f3"].iter().map(|s| s.to_string()).collect();
        let params = parse_object_params(&obj, &targets);
        let f2p = params.iter().find(|(n, _)| n == "f2").expect("f2 params");
        assert_eq!(f2p.1.len(), 1, "f2 has one param");
        assert_eq!(f2p.1[0].0, "x", "param is named x");
        assert!(
            matches!(f2p.1[0].1, BtParamLoc::Fbreg(_)),
            "x is spilled to the frame: {:?}",
            f2p.1[0].1
        );
    }
}
