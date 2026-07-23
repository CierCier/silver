use std::path::Path;

use inkwell::context::Context;
use inkwell::debug_info::{
    AsDIScope, DIBasicType, DICompileUnit, DIFile, DIFlags, DIFlagsConstants, DILexicalBlock,
    DILocation, DIScope, DISubprogram, DISubroutineType, DIType, DebugInfoBuilder,
    LLVMDWARFTypeEncoding, debug_metadata_version,
};
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
    pub di_file: DIFile<'ctx>,
    pub source_map: SourceMap,
    pub di_types: rustc_hash::FxHashMap<String, DIType<'ctx>>,
    pub current_scope: DIScope<'ctx>,
    pub current_subprogram: Option<DISubprogram<'ctx>>,
    pub current_lexical_blocks: Vec<DILexicalBlock<'ctx>>,
}

impl<'ctx> DebugContext<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: &Module<'ctx>,
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

        Self {
            dibuilder,
            compile_unit,
            di_file,
            source_map,
            di_types: rustc_hash::FxHashMap::default(),
            current_scope: compile_unit.as_debug_info_scope(),
            current_subprogram: None,
            current_lexical_blocks: Vec::new(),
        }
    }

    pub fn current_scope(&self) -> DIScope<'ctx> {
        if let Some(block) = self.current_lexical_blocks.last() {
            block.as_debug_info_scope()
        } else if let Some(sub) = &self.current_subprogram {
            sub.as_debug_info_scope()
        } else {
            self.compile_unit.as_debug_info_scope()
        }
    }

    pub fn push_lexical_block(&mut self, line: u32, col: u32) {
        let scope = self.current_scope();
        let block = self
            .dibuilder
            .create_lexical_block(scope, self.di_file, line, col);
        self.current_lexical_blocks.push(block);
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
        encoding: LLVMDWARFTypeEncoding,
    ) -> Result<DIBasicType<'ctx>, inkwell::error::Error> {
        self.dibuilder
            .create_basic_type(name, size_in_bits, encoding, DIFlags::PUBLIC)
    }

    pub fn create_subroutine_type(
        &mut self,
        return_type: Option<DIType<'ctx>>,
        param_types: &[DIType<'ctx>],
    ) -> DISubroutineType<'ctx> {
        self.dibuilder.create_subroutine_type(
            self.di_file,
            return_type,
            param_types,
            DIFlags::PUBLIC,
        )
    }

    pub fn create_function(
        &mut self,
        name: &str,
        linkage_name: &str,
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
            self.di_file,
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
