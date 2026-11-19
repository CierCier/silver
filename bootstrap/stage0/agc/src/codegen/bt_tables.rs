//! The runtime's exact-line and argument tables (`__silver_bt_lines`,
//! `__silver_bt_args`), resolved from an emitted object's DWARF, and the ELF
//! writer that stores them into that same object.
//!
//! The tables need machine-code offsets, which exist only after LLVM emits
//! the object. Re-emitting the whole module with the tables added doubled
//! machine-code generation for every debug build. On ELF the compiler instead
//! reserves the table symbols in their own section before the emit, then
//! rewrites only that section and its relocations: function addresses are
//! relocations against the object's own function symbols (local ones
//! included) or, for functions a split module emitted into a sibling object,
//! against their undefined symbols; everything else is relative to the
//! section symbol.

use rustc_hash::FxHashMap as HashMap;

use crate::codegen::dwarf_bt::{BtFnDebug, BtLineEntry, BtParamLoc};

/// Section that holds the reserved table symbols in an ELF object.
pub(crate) const BT_SECTION: &str = ".data.silver_bt";

/// Runtime-visible table symbols, in the order the writers lay them out.
pub(crate) const BT_LINES: &str = "__silver_bt_lines";
pub(crate) const BT_LINES_COUNT: &str = "__silver_bt_lines_count";
pub(crate) const BT_ARGS: &str = "__silver_bt_args";
pub(crate) const BT_ARGS_COUNT: &str = "__silver_bt_args_count";

/// One spilled argument the runtime can print: its name, `DW_OP_fbreg`
/// frame offset, and byte size (at most 8).
pub(crate) struct BtArg {
    pub name: String,
    pub fbreg: i64,
    pub size: u64,
}

/// One function's table rows.
pub(crate) struct BtFunction {
    pub name: String,
    pub lines: Vec<BtLineEntry>,
    /// Empty when no argument lives in the frame.
    pub args: Vec<BtArg>,
}

/// Tables for every function the module defines.
#[derive(Default)]
pub(crate) struct BtTables {
    pub functions: Vec<BtFunction>,
}

impl BtTables {
    /// Keeps the functions `param_sizes` knows (it returns each source-level
    /// parameter's ABI size) and the arguments that live in the frame.
    pub(crate) fn resolve(
        fn_debug: Vec<BtFnDebug>,
        mut param_sizes: impl FnMut(&str) -> Option<Vec<u64>>,
    ) -> Self {
        let functions = fn_debug
            .into_iter()
            .filter_map(|function| {
                let sizes = param_sizes(&function.name)?;
                let args = function
                    .params
                    .into_iter()
                    .enumerate()
                    .filter_map(|(index, (name, location))| match location {
                        BtParamLoc::Fbreg(fbreg) => Some(BtArg {
                            name,
                            fbreg,
                            size: sizes.get(index).copied().unwrap_or(8).min(8),
                        }),
                        BtParamLoc::Reg(_) | BtParamLoc::Unknown => None,
                    })
                    .collect();
                Some(BtFunction {
                    name: function.name,
                    lines: function.lines,
                    args,
                })
            })
            .collect();
        Self { functions }
    }

    pub(crate) fn line_count(&self) -> usize {
        self.functions
            .iter()
            .map(|function| function.lines.len())
            .sum()
    }

    pub(crate) fn functions_with_args(&self) -> impl Iterator<Item = &BtFunction> {
        self.functions
            .iter()
            .filter(|function| !function.args.is_empty())
    }
}

const SHT_PROGBITS: u32 = 1;
const SHT_SYMTAB: u32 = 2;
const SHT_RELA: u32 = 4;
const STT_FUNC: u8 = 2;
const STT_SECTION: u8 = 3;
const SHN_UNDEF: usize = 0;
const SECTION_HEADER_OFFSET_FIELD: usize = 24;
const SECTION_HEADER_SIZE_FIELD: usize = 32;
const SYMBOL_SIZE: usize = 24;
const RELA_SIZE: usize = 24;
const LINE_RECORD_SIZE: u64 = 32;
const ARGS_RECORD_SIZE: u64 = 24;
const ARG_RECORD_SIZE: u64 = 24;

struct Section {
    header: usize,
    ty: u32,
    offset: usize,
    size: usize,
    link: u32,
    info: u32,
}

/// Returns `obj` with `tables` written into its reserved [`BT_SECTION`], or
/// `None` when the object is not a 64-bit little-endian ELF object with the
/// reservation intact (the caller then re-emits the module with the tables).
pub(crate) fn write_into_elf(obj: &[u8], tables: &BtTables) -> Option<Vec<u8>> {
    if !obj.starts_with(b"\x7fELF\x02\x01\x01") {
        return None;
    }
    let section_headers = read_u64(obj, 0x28)? as usize;
    let header_size = read_u16(obj, 0x3a)? as usize;
    let section_count = read_u16(obj, 0x3c)? as usize;
    let names_index = read_u16(obj, 0x3e)? as usize;
    // Extended numbering keeps the real counts in section 0; bail out.
    if header_size < 64 || section_count == 0 || names_index == 0xffff {
        return None;
    }
    let sections = (0..section_count)
        .map(|index| {
            let header = section_headers + index * header_size;
            Some(Section {
                header,
                ty: read_u32(obj, header + 4)?,
                offset: read_u64(obj, header + SECTION_HEADER_OFFSET_FIELD)? as usize,
                size: read_u64(obj, header + SECTION_HEADER_SIZE_FIELD)? as usize,
                link: read_u32(obj, header + 40)?,
                info: read_u32(obj, header + 44)?,
            })
        })
        .collect::<Option<Vec<_>>>()?;
    let names = sections.get(names_index)?;
    let section_name = |section: &Section| -> Option<&[u8]> {
        let name_offset = read_u32(obj, section.header)? as usize;
        c_str(obj, names.offset + name_offset)
    };
    let table_index = sections
        .iter()
        .position(|section| section_name(section) == Some(BT_SECTION.as_bytes()))?;
    if sections[table_index].ty != SHT_PROGBITS {
        return None;
    }
    let mut relocation_sections = sections
        .iter()
        .filter(|section| section.ty == SHT_RELA && section.info as usize == table_index);
    let relocations = relocation_sections.next()?;
    if relocation_sections.next().is_some() {
        return None;
    }
    let symtab = sections.get(relocations.link as usize)?;
    if symtab.ty != SHT_SYMTAB {
        return None;
    }
    let strtab = sections.get(symtab.link as usize)?;

    let mut section_symbol = None;
    let mut table_symbols: HashMap<&[u8], u64> = HashMap::default();
    let mut functions: HashMap<&[u8], u64> = HashMap::default();
    for index in 0..symtab.size / SYMBOL_SIZE {
        let entry = symtab.offset + index * SYMBOL_SIZE;
        let kind = *obj.get(entry + 4)? & 0xf;
        let shndx = read_u16(obj, entry + 6)? as usize;
        let name = c_str(obj, strtab.offset + read_u32(obj, entry)? as usize)?;
        if shndx == table_index {
            if kind == STT_SECTION {
                section_symbol = Some(index as u64);
            } else {
                table_symbols.insert(name, read_u64(obj, entry + 8)?);
            }
        } else if (kind == STT_FUNC && shndx != SHN_UNDEF) || (shndx == SHN_UNDEF && index != 0) {
            // Undefined: a function defined in another object of a split
            // module, which the linker resolves.
            functions.insert(name, index as u64);
        }
    }
    let section_symbol = section_symbol?;
    let slot = |name: &str| table_symbols.get(name.as_bytes()).copied();
    let (lines_slot, lines_count_slot, args_slot, args_count_slot) = (
        slot(BT_LINES)?,
        slot(BT_LINES_COUNT)?,
        slot(BT_ARGS)?,
        slot(BT_ARGS_COUNT)?,
    );
    if relocations.size < RELA_SIZE {
        return None;
    }
    let mut relas = Relocations {
        bytes: Vec::new(),
        // Copy the data relocation LLVM chose for the reserved pointers
        // (R_X86_64_64, R_AARCH64_ABS64, ...) instead of hard-coding one.
        kind: read_u64(obj, relocations.offset + 8)? & 0xffff_ffff,
    };
    let function_symbols = tables
        .functions
        .iter()
        .map(|function| functions.get(function.name.as_bytes()).copied())
        .collect::<Option<Vec<_>>>()?;

    // Layout: reserved slots, line records, per-function argument records,
    // argument arrays, then deduplicated C strings.
    let table = &sections[table_index];
    let mut data = obj.get(table.offset..table.offset + table.size)?.to_vec();
    data.resize(data.len().next_multiple_of(8), 0);
    let line_count = tables.line_count() as u64;
    let args_count = tables.functions_with_args().count() as u64;
    let arg_count: u64 = tables
        .functions_with_args()
        .map(|function| function.args.len() as u64)
        .sum();
    let lines_at = data.len() as u64;
    let args_at = lines_at + LINE_RECORD_SIZE * line_count;
    let arg_arrays_at = args_at + ARGS_RECORD_SIZE * args_count;
    let mut strings = Strings {
        base: arg_arrays_at + ARG_RECORD_SIZE * arg_count,
        bytes: Vec::new(),
        offsets: HashMap::default(),
    };
    let push = |data: &mut Vec<u8>, value: u64| data.extend_from_slice(&value.to_le_bytes());

    for (function, &symbol) in tables.functions.iter().zip(&function_symbols) {
        for line in &function.lines {
            let at = data.len() as u64;
            relas.push(at, symbol, 0);
            push(&mut data, 0);
            push(&mut data, line.offset);
            push(&mut data, line.line);
            relas.push(at + 24, section_symbol, strings.at(&line.file));
            push(&mut data, 0);
        }
    }
    let mut array_at = arg_arrays_at;
    for (function, &symbol) in tables.functions.iter().zip(&function_symbols) {
        if function.args.is_empty() {
            continue;
        }
        let at = data.len() as u64;
        relas.push(at, symbol, 0);
        push(&mut data, 0);
        push(&mut data, function.args.len() as u64);
        relas.push(at + 16, section_symbol, array_at);
        push(&mut data, 0);
        array_at += ARG_RECORD_SIZE * function.args.len() as u64;
    }
    for arg in tables
        .functions_with_args()
        .flat_map(|function| &function.args)
    {
        let at = data.len() as u64;
        relas.push(at, section_symbol, strings.at(&arg.name));
        push(&mut data, 0);
        push(&mut data, arg.fbreg as u64);
        push(&mut data, arg.size);
    }
    debug_assert_eq!(data.len() as u64, strings.base);
    data.extend_from_slice(&strings.bytes);

    // RELA ignores the bytes at a relocated slot, so every slot holds 0 and
    // the pointers live entirely in the relocations.
    if line_count > 0 {
        relas.push(lines_slot, section_symbol, lines_at);
    }
    if args_count > 0 {
        relas.push(args_slot, section_symbol, args_at);
    }
    for (slot, value) in [
        (lines_slot, 0),
        (lines_count_slot, line_count),
        (args_slot, 0),
        (args_count_slot, args_count),
    ] {
        let slot = slot as usize;
        data.get_mut(slot..slot + 8)?
            .copy_from_slice(&value.to_le_bytes());
    }

    // Append the new contents and point the two section headers at them;
    // the old bytes stay behind unreferenced.
    let mut out = obj.to_vec();
    out.resize(out.len().next_multiple_of(8), 0);
    let data_offset = out.len();
    out.extend_from_slice(&data);
    out.resize(out.len().next_multiple_of(8), 0);
    let relas_offset = out.len();
    out.extend_from_slice(&relas.bytes);
    for (header, offset, size) in [
        (table.header, data_offset, data.len()),
        (relocations.header, relas_offset, relas.bytes.len()),
    ] {
        out[header + SECTION_HEADER_OFFSET_FIELD..][..8]
            .copy_from_slice(&(offset as u64).to_le_bytes());
        out[header + SECTION_HEADER_SIZE_FIELD..][..8]
            .copy_from_slice(&(size as u64).to_le_bytes());
    }
    Some(out)
}

/// `Elf64_Rela` entries of one relocation kind.
struct Relocations {
    bytes: Vec<u8>,
    kind: u64,
}

impl Relocations {
    fn push(&mut self, at: u64, symbol: u64, addend: u64) {
        self.bytes.extend_from_slice(&at.to_le_bytes());
        self.bytes
            .extend_from_slice(&(symbol << 32 | self.kind).to_le_bytes());
        self.bytes.extend_from_slice(&addend.to_le_bytes());
    }
}

/// Deduplicated C strings placed at `base` within the table section.
struct Strings<'a> {
    base: u64,
    bytes: Vec<u8>,
    offsets: HashMap<&'a str, u64>,
}

impl<'a> Strings<'a> {
    fn at(&mut self, text: &'a str) -> u64 {
        *self.offsets.entry(text).or_insert_with(|| {
            let at = self.base + self.bytes.len() as u64;
            self.bytes.extend_from_slice(text.as_bytes());
            self.bytes.push(0);
            at
        })
    }
}

fn read_u16(data: &[u8], at: usize) -> Option<u16> {
    Some(u16::from_le_bytes(data.get(at..at + 2)?.try_into().ok()?))
}

fn read_u32(data: &[u8], at: usize) -> Option<u32> {
    Some(u32::from_le_bytes(data.get(at..at + 4)?.try_into().ok()?))
}

fn read_u64(data: &[u8], at: usize) -> Option<u64> {
    Some(u64::from_le_bytes(data.get(at..at + 8)?.try_into().ok()?))
}

fn c_str(data: &[u8], at: usize) -> Option<&[u8]> {
    let rest = data.get(at..)?;
    Some(&rest[..rest.iter().position(|byte| *byte == 0)?])
}
