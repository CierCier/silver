use std::cell::RefCell;
use std::fmt;
use std::fs;
use std::time::Instant;

use owo_colors::OwoColorize;

#[derive(Debug, Clone)]
pub struct PhaseRecord {
    pub name: String,
    pub elapsed_ms: f64,
    pub memory_before_kb: u64,
    pub memory_after_kb: u64,
    /// Nesting depth; 0 = top-level phase, >0 = sub-phase (per-file detail).
    pub depth: usize,
}

impl PhaseRecord {
    pub fn memory_delta_kb(&self) -> i64 {
        self.memory_after_kb as i64 - self.memory_before_kb as i64
    }
}

#[derive(Debug)]
struct PhaseFrame {
    name: String,
    start: Instant,
    memory_before_kb: u64,
}

/// A node in the phase tree, reconstructed from the flat records.
struct TreeNode {
    record: PhaseRecord,
    children: Vec<TreeNode>,
}

#[derive(Debug, Default)]
pub struct Profiler {
    pub enabled: bool,
    pub verbose: bool,
    pub records: Vec<PhaseRecord>,
    start_time: Option<Instant>,
    total_memory_before_kb: u64,
    stack: Vec<PhaseFrame>,
}

impl Profiler {
    pub fn new(enabled: bool, verbose: bool) -> Self {
        Self {
            enabled,
            verbose,
            records: Vec::new(),
            start_time: None,
            total_memory_before_kb: 0,
            stack: Vec::new(),
        }
    }

    pub fn begin(&mut self) {
        if !self.enabled {
            return;
        }
        self.start_time = Some(Instant::now());
        self.total_memory_before_kb = read_rss_kb().unwrap_or(0);
    }

    pub fn begin_phase(&mut self, name: &str) {
        if !self.enabled {
            return;
        }
        if self.start_time.is_none() {
            self.begin();
        }
        self.stack.push(PhaseFrame {
            name: name.to_string(),
            start: Instant::now(),
            memory_before_kb: read_rss_kb().unwrap_or(0),
        });
    }

    pub fn end_phase(&mut self, name: &str) {
        if !self.enabled {
            return;
        }
        // Pop the frame for this phase; tolerate mismatched name (just pop top).
        let frame = match self.stack.iter().rposition(|f| f.name == name) {
            Some(idx) => self.stack.remove(idx),
            None => match self.stack.pop() {
                Some(f) => f,
                None => return,
            },
        };
        let elapsed_ms = frame.start.elapsed().as_secs_f64() * 1000.0;
        let memory_after_kb = read_rss_kb().unwrap_or(0);
        self.records.push(PhaseRecord {
            name: frame.name,
            elapsed_ms,
            memory_before_kb: frame.memory_before_kb,
            memory_after_kb,
            depth: self.stack.len(),
        });
    }

    fn render_report_line(&self, name: &str, width: usize, elapsed_ms: f64, before: u64, after: u64) -> String {
        let delta = after as i64 - before as i64;
        let delta_str = if delta >= 0 {
            format!("+{delta}")
        } else {
            format!("{delta}")
        };
        format!(
            "{name:<width$} {elapsed_ms:>10.2} {before:>10} KB {after:>10} KB {delta_str:>10} KB"
        )
    }

    /// Build the phase tree from flat records (which arrive children-first,
    /// each phase ending before its parent). A record's children are the
    /// records immediately preceding it whose depth is greater.
    fn build_tree(records: &[PhaseRecord]) -> Vec<TreeNode> {
        let mut nodes: Vec<TreeNode> = Vec::new();
        for record in records {
            let depth = record.depth;
            let mut child_start = nodes.len();
            while child_start > 0 && nodes[child_start - 1].record.depth > depth {
                child_start -= 1;
            }
            let children: Vec<TreeNode> = nodes.drain(child_start..).collect();
            // drain preserves arrival (sibling) order, so no reversal needed.
            nodes.push(TreeNode {
                record: record.clone(),
                children,
            });
        }
        nodes
    }

    /// Collect (tree-prefixed name, timings, is_sub) pairs for every record.
    fn collect_tree(
        &self,
        nodes: &[TreeNode],
        prefix: &str,
        root_level: bool,
        out: &mut Vec<(String, f64, u64, u64, bool)>,
    ) {
        for (i, node) in nodes.iter().enumerate() {
            let is_last = i + 1 == nodes.len();
            let connector = if root_level {
                String::new()
            } else if is_last {
                "└── ".to_string()
            } else {
                "├── ".to_string()
            };
            let child_prefix = if root_level {
                String::new()
            } else if is_last {
                format!("{prefix}    ")
            } else {
                format!("{prefix}│   ")
            };
            let name = format!("{prefix}{connector}{}", node.record.name);
            out.push((
                name,
                node.record.elapsed_ms,
                node.record.memory_before_kb,
                node.record.memory_after_kb,
                !root_level,
            ));
            self.collect_tree(&node.children, &child_prefix, false, out);
        }
    }

    /// Report entries plus the fixed width of the name column, so the time and
    /// memory columns align regardless of tree depth or name length.
    fn report_entries(&self) -> (Vec<(String, f64, u64, u64, bool)>, usize) {
        let mut entries = Vec::new();
        self.collect_tree(&Self::build_tree(&self.records), "", true, &mut entries);
        let width = entries
            .iter()
            .map(|(name, _, _, _, _)| name.len())
            .max()
            .unwrap_or(24)
            .max(24);
        (entries, width)
    }

    fn report_lines(&self) -> (Vec<(String, bool)>, usize) {
        let (entries, width) = self.report_entries();
        let lines = entries
            .into_iter()
            .map(|(name, elapsed_ms, before, after, sub)| {
                (self.render_report_line(&name, width, elapsed_ms, before, after), sub)
            })
            .collect();
        (lines, width)
    }

    pub fn print_report(&self) {
        if !self.enabled || self.records.is_empty() {
            return;
        }

        let total_time_ms: f64 = self.records.iter().map(|r| r.elapsed_ms).sum();
        let total_mem_delta_kb: i64 = self.records.iter().map(|r| r.memory_delta_kb()).sum();
        let peak_mem_kb = self
            .records
            .iter()
            .map(|r| r.memory_after_kb)
            .max()
            .unwrap_or(0);

        eprintln!(
            "\n{}",
            "=== Compiler Profile Report ===".bright_cyan().bold()
        );
        let (lines, width) = self.report_lines();
        let rule = "─".repeat(width + 1 + 10 + 1 + 13 + 1 + 13 + 1 + 13);
        eprintln!(
            "{:<width$} {:>10} {:>13} {:>13} {:>13}",
            "Phase", "Time (ms)", "Mem Before", "Mem After", "Delta (KB)"
        );
        eprintln!("{}", rule.bright_black());

        for (line, sub) in lines {
            // Nested phases are dimmed to keep the top-level stages readable.
            if sub {
                eprintln!("{}", line.bright_black());
            } else {
                eprintln!("{line}");
            }
        }

        eprintln!("{}", rule.bright_black());
        eprintln!(
            "{:<width$} {:>10.2} {:>34} {:>10} KB",
            "Total", total_time_ms, "", peak_mem_kb,
        );
        eprintln!(
            "{:<width$} {:>36} {:>10} KB",
            "",
            "",
            format!("net: {total_mem_delta_kb:+}"),
        );
        eprintln!();
    }
}

thread_local! {
    static CURRENT: RefCell<Option<Profiler>> = const { RefCell::new(None) };
}

/// Install the process-wide profiler (replaces any previous one).
pub fn install(profiler: Profiler) {
    CURRENT.with(|current| *current.borrow_mut() = Some(profiler));
}

/// Run `f` with the installed profiler; runs with a disabled default when
/// none was installed (e.g. in tests or library embedding).
fn with_profiler<R>(f: impl FnOnce(&mut Profiler) -> R) -> R {
    CURRENT.with(|current| {
        let mut guard = current.borrow_mut();
        let profiler = guard.get_or_insert_with(|| Profiler::new(false, false));
        f(profiler)
    })
}

/// True when verbose profiling is active (--profile --verbose).
pub fn verbose() -> bool {
    with_profiler(|p| p.verbose)
}

pub fn enabled() -> bool {
    with_profiler(|p| p.enabled)
}

pub fn begin() {
    with_profiler(|p| p.begin());
}

pub fn begin_phase(name: &str) {
    with_profiler(|p| p.begin_phase(name));
}

pub fn end_phase(name: &str) {
    with_profiler(|p| p.end_phase(name));
}

pub fn print_report() {
    with_profiler(|p| p.print_report());
}

pub fn read_rss_kb() -> Option<u64> {
    let status = fs::read_to_string("/proc/self/status").ok()?;
    for line in status.lines() {
        if let Some(rest) = line.strip_prefix("VmRSS:") {
            return rest.split_whitespace().next()?.parse().ok();
        }
    }
    None
}

pub fn format_bytes(kb: u64) -> String {
    if kb < 1024 {
        format!("{kb} KB")
    } else if kb < 1024 * 1024 {
        format!("{:.1} MB", kb as f64 / 1024.0)
    } else {
        format!("{:.2} GB", kb as f64 / (1024.0 * 1024.0))
    }
}

impl fmt::Display for Profiler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.enabled || self.records.is_empty() {
            return Ok(());
        }

        let total_time_ms: f64 = self.records.iter().map(|r| r.elapsed_ms).sum();
        let total_mem_delta_kb: i64 = self.records.iter().map(|r| r.memory_delta_kb()).sum();
        let peak_mem_kb = self
            .records
            .iter()
            .map(|r| r.memory_after_kb)
            .max()
            .unwrap_or(0);

        writeln!(f, "\n=== Compiler Profile Report ===")?;
        let (lines, width) = self.report_lines();
        let rule = "─".repeat(width + 1 + 10 + 1 + 13 + 1 + 13 + 1 + 13);
        writeln!(
            f,
            "{:<width$} {:>10} {:>13} {:>13} {:>13}",
            "Phase", "Time (ms)", "Mem Before", "Mem After", "Delta (KB)"
        )?;
        writeln!(f, "{rule}")?;

        for (line, _sub) in lines {
            writeln!(f, "{line}")?;
        }

        writeln!(f, "{rule}")?;
        writeln!(
            f,
            "{:<width$} {:>10.2} {:>34} {:>10} KB",
            "Total", total_time_ms, "", peak_mem_kb,
        )?;
        writeln!(
            f,
            "{:<width$} {:>36} {:>10} KB",
            "",
            "",
            format!("net: {total_mem_delta_kb:+}"),
        )?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rec(name: &str, depth: usize) -> PhaseRecord {
        PhaseRecord {
            name: name.to_string(),
            elapsed_ms: 1.0,
            memory_before_kb: 0,
            memory_after_kb: 0,
            depth,
        }
    }

    #[test]
    fn builds_tree_from_children_first_records() {
        // Arrival (children-first) order.
        let records = vec![
            rec("parse", 0),
            rec("read test.ag", 2),
            rec("lex test.ag", 2),
            rec("parse test.ag", 2),
            rec("read memory.ag", 3),
            rec("lex memory.ag", 3),
            rec("parse memory.ag", 3),
            rec("import memory.ag", 2),
            rec("import test.ag", 1),
            rec("import lowering", 0),
            rec("semantic", 0),
        ];
        let tree = Profiler::build_tree(&records);
        fn names(nodes: &[TreeNode]) -> Vec<&str> {
            nodes.iter().map(|n| n.record.name.as_str()).collect()
        }
        assert_eq!(names(&tree), vec!["parse", "import lowering", "semantic"]);
        let il = &tree[1];
        assert_eq!(names(&il.children), vec!["import test.ag"]);
        let it = &il.children[0];
        assert_eq!(
            names(&it.children),
            vec!["read test.ag", "lex test.ag", "parse test.ag", "import memory.ag"]
        );
        let im = &it.children[3];
        assert_eq!(names(&im.children), vec!["read memory.ag", "lex memory.ag", "parse memory.ag"]);
    }
}
