use std::path::Path;

use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DIBasicType, DICompileUnit, DIFile, DIFlags, DIFlagsConstants, DILexicalBlock,
    DILocation, DIScope, DISubprogram, DISubroutineType, DIType, DebugInfoBuilder,
    debug_metadata_version,
};

/// DWARF base-type encodings (DW_ATE_*; llvm-sys exposes the raw c_uint).
pub(crate) mod ate {
    pub const BOOLEAN: u32 = 0x02;
    pub const FLOAT: u32 = 0x04;
    pub const SIGNED: u32 = 0x05;
    pub const SIGNED_CHAR: u32 = 0x06;
    pub const UNSIGNED: u32 = 0x07;
    pub const UNSIGNED_CHAR: u32 = 0x08;
}
use inkwell::module::{FlagBehavior, Module};

use crate::lexer::Span;

#[derive(Debug, Clone)]
pub struct SourceMap {
    line_starts: Vec<usize>,
}

impl SourceMap {
    pub fn new(source: &str) -> Self {
        let mut line_starts = Vec::new();
        line_starts.push(0);
        for (i, b) in source.bytes().enumerate() {
            if b == b'\n' {
                line_starts.push(i + 1);
            }
        }
        Self { line_starts }
    }

    pub fn span_to_line_col(&self, span: &Span) -> (u32, u32, u32, u32) {
        let (start_line, start_col) = self.byte_offset_to_line_col(span.start);
        let (end_line, end_col) = self.byte_offset_to_line_col(span.end);
        (start_line, start_col, end_line, end_col)
    }

    pub fn byte_offset_to_line_col(&self, offset: usize) -> (u32, u32) {
        match self.line_starts.binary_search(&offset) {
            Ok(line) => (line as u32 + 1, 1),
            Err(insertion_point) => {
                let line = insertion_point.saturating_sub(1);
                let col = offset.saturating_sub(self.line_starts[line]) + 1;
                (line as u32 + 1, col as u32)
            }
        }
    }

    pub fn line_count(&self) -> usize {
        self.line_starts.len()
    }
}

pub struct DebugContext<'ctx> {
    pub dibuilder: DebugInfoBuilder<'ctx>,
    pub compile_unit: DICompileUnit<'ctx>,
    /// Per-source-file (DIFile, SourceMap), keyed by the lexer source
    /// registry file id. Id 0 is the main compilation unit. Files are
    /// registered lazily when a span from an inlined import is first seen.
    pub files: rustc_hash::FxHashMap<u32, (DIFile<'ctx>, SourceMap)>,
    pub di_types: rustc_hash::FxHashMap<String, DIType<'ctx>>,
    /// Type keys currently being built (guards recursive struct types).
    pub building: rustc_hash::FxHashSet<String>,
    pub current_scope: DIScope<'ctx>,
    pub current_subprogram: Option<DISubprogram<'ctx>>,
    /// Lexical blocks keyed by the subprogram that owns them. Functions
    /// emitted lazily mid-codegen (generic instances) push their own blocks;
    /// the owner tag keeps the enclosing function's blocks from leaking into
    /// their scope chain (dangling DIE parents crash DWARF emission).
    pub current_lexical_blocks: Vec<(DILexicalBlock<'ctx>, Option<DISubprogram<'ctx>>)>,
}

impl<'ctx> DebugContext<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: &Module<'ctx>,
        main_file_id: u32,
        source_path: &Path,
        source_text: &str,
    ) -> Self {
        let debug_metadata_version = context
            .i32_type()
            .const_int(debug_metadata_version() as u64, false);
        module.add_basic_value_flag(
            "Debug Info Version",
            FlagBehavior::Warning,
            debug_metadata_version,
        );

        let filename = source_path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown.ag");
        let directory = source_path.parent().and_then(|p| p.to_str()).unwrap_or(".");

        let (dibuilder, compile_unit) = module.create_debug_info_builder(
            true,
            inkwell::debug_info::DWARFSourceLanguage::C,
            filename,
            directory,
            "agc",
            false,
            "",
            0,
            "",
            inkwell::debug_info::DWARFEmissionKind::Full,
            0,
            false,
            false,
            "",
            "",
        );

        let di_file = dibuilder.create_file(filename, directory);
        let source_map = SourceMap::new(source_text);
        let mut files = rustc_hash::FxHashMap::default();
        files.insert(main_file_id, (di_file, source_map));

        Self {
            dibuilder,
            compile_unit,
            files,
            di_types: rustc_hash::FxHashMap::default(),
            building: rustc_hash::FxHashSet::default(),
            current_scope: compile_unit.as_debug_info_scope(),
            current_subprogram: None,
            current_lexical_blocks: Vec::new(),
        }
    }

    /// DIFile for a span's source file, lazily registering inlined imports
    /// from the lexer's source registry.
    pub fn file_for(&mut self, span: &Span) -> DIFile<'ctx> {
        if let Some((file, _)) = self.files.get(&span.file) {
            return *file;
        }
        if let Some(source) = crate::lexer::source_file(span.file) {
            let path = Path::new(&source.path);
            let filename = path
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("unknown.ag");
            let directory = path.parent().and_then(|p| p.to_str()).unwrap_or(".");
            let file = self.dibuilder.create_file(filename, directory);
            let source_map = SourceMap::new(&source.text);
            self.files.insert(span.file, (file, source_map));
            return file;
        }
        // Unregistered span: fall back to the main compilation unit.
        self.files
            .get(&0)
            .map(|(file, _)| *file)
            .or_else(|| self.files.values().next().map(|(file, _)| *file))
            .expect("debug context has at least one file")
    }

    /// Line/column for a span in its own file (byte-exact for registered
    /// files; lexer-cached numbers otherwise).
    pub fn span_to_line_col(&self, span: &Span) -> (u32, u32, u32, u32) {
        if let Some((_, source_map)) = self.files.get(&span.file) {
            return source_map.span_to_line_col(span);
        }
        (
            span.start_line.max(1),
            span.start_col.max(1),
            span.end_line.max(1),
            span.end_col.max(1),
        )
    }

    /// DIFile of the main compilation unit (fallback for type metadata that
    /// has no per-span file).
    pub fn main_file(&self) -> DIFile<'ctx> {
        self.files
            .get(&0)
            .map(|(file, _)| *file)
            .or_else(|| self.files.values().next().map(|(file, _)| *file))
            .expect("debug context has at least one file")
    }

    pub fn current_scope(&self) -> DIScope<'ctx> {
        if let Some((block, owner)) = self.current_lexical_blocks.last() {
            // Blocks belonging to a different (outer) subprogram are invisible
            // while a lazily-emitted generic instance is being generated; they
            // become visible again once that function finishes (subprogram is
            // reset to None) and the enclosing codegen resumes.
            if let Some(sub) = self.current_subprogram
                && *owner != self.current_subprogram
            {
                return sub.as_debug_info_scope();
            }
            return block.as_debug_info_scope();
        }
        if let Some(sub) = &self.current_subprogram {
            sub.as_debug_info_scope()
        } else {
            self.compile_unit.as_debug_info_scope()
        }
    }

    pub fn push_lexical_block(&mut self, span: &Span, line: u32, col: u32) {
        let scope = self.current_scope();
        let file = self.file_for(span);
        let block = self.dibuilder.create_lexical_block(scope, file, line, col);
        let owner = self.current_subprogram;
        self.current_lexical_blocks.push((block, owner));
    }

    pub fn pop_lexical_block(&mut self) {
        self.current_lexical_blocks.pop();
    }

    pub fn create_debug_location(
        &self,
        context: &'ctx Context,
        line: u32,
        col: u32,
    ) -> DILocation<'ctx> {
        let scope = self.current_scope();
        self.dibuilder
            .create_debug_location(context, line, col, scope, None)
    }

    pub fn create_basic_type(
        &mut self,
        name: &str,
        size_in_bits: u64,
        encoding: u32,
    ) -> Result<DIBasicType<'ctx>, inkwell::error::Error> {
        self.dibuilder
            .create_basic_type(name, size_in_bits, encoding, DIFlags::PUBLIC)
    }

    /// Cached unsigned-char basic type; fallback for pointers to types that
    /// cannot be described (recursive placeholders etc.).
    pub fn byte_type(&mut self) -> DIType<'ctx> {
        const KEY: &str = "$byte";
        if let Some(t) = self.di_types.get(KEY) {
            return *t;
        }
        let t = self
            .dibuilder
            .create_basic_type("u8", 8, ate::UNSIGNED_CHAR, DIFlags::PUBLIC)
            .expect("create u8 basic type");
        self.di_types.insert(KEY.to_string(), t.as_type());
        t.as_type()
    }

    pub fn create_subroutine_type(
        &mut self,
        file: DIFile<'ctx>,
        return_type: Option<DIType<'ctx>>,
        param_types: &[DIType<'ctx>],
    ) -> DISubroutineType<'ctx> {
        self.dibuilder
            .create_subroutine_type(file, return_type, param_types, DIFlags::PUBLIC)
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "codegen context threading; a config struct would hide more than it clarifies"
    )]
    pub fn create_function(
        &mut self,
        name: &str,
        linkage_name: &str,
        file: DIFile<'ctx>,
        line: u32,
        subroutine_type: DISubroutineType<'ctx>,
        is_local: bool,
        is_definition: bool,
        scope_line: u32,
    ) -> DISubprogram<'ctx> {
        self.dibuilder.create_function(
            self.compile_unit.as_debug_info_scope(),
            name,
            Some(linkage_name),
            file,
            line,
            subroutine_type,
            is_local,
            is_definition,
            scope_line,
            DIFlags::PUBLIC,
            false,
        )
    }

    pub fn finalize(self) {
        self.dibuilder.finalize();
    }
}
