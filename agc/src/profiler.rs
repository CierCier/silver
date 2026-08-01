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

    fn render_report_line(&self, name: &str, elapsed_ms: f64, before: u64, after: u64) -> String {
        let delta = after as i64 - before as i64;
        let delta_str = if delta >= 0 {
            format!("+{delta}")
        } else {
            format!("{delta}")
        };
        format!("{name:<24} {elapsed_ms:>10.2} {before:>10} KB {after:>10} KB {delta_str:>10} KB")
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
        eprintln!(
            "{:<24} {:>10} {:>12} {:>12} {:>12}",
            "Phase", "Time (ms)", "Mem Before", "Mem After", "Delta (KB)"
        );
        eprintln!("{}", "-".repeat(74).bright_black());

        for record in &self.records {
            let indent = "  ".repeat(record.depth);
            let line = self.render_report_line(
                &format!("{indent}{}", record.name),
                record.elapsed_ms,
                record.memory_before_kb,
                record.memory_after_kb,
            );
            // Sub-phases get dimmed to keep the top-level phases readable.
            if record.depth > 0 {
                eprintln!("{}", line.bright_black());
            } else {
                eprintln!("{line}");
            }
        }

        eprintln!("{}", "-".repeat(74).bright_black());
        eprintln!(
            "{:<24} {:>10.2} {:>34} {:>10} KB",
            "Total", total_time_ms, "", peak_mem_kb,
        );
        eprintln!(
            "{:<24} {:>36} {:>10} KB",
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
        writeln!(
            f,
            "{:<24} {:>10} {:>12} {:>12} {:>12}",
            "Phase", "Time (ms)", "Mem Before", "Mem After", "Delta (KB)"
        )?;
        writeln!(f, "{}", "-".repeat(74))?;

        for record in &self.records {
            let indent = "  ".repeat(record.depth);
            let line = self.render_report_line(
                &format!("{indent}{}", record.name),
                record.elapsed_ms,
                record.memory_before_kb,
                record.memory_after_kb,
            );
            writeln!(f, "{line}")?;
        }

        writeln!(f, "{}", "-".repeat(74))?;
        writeln!(
            f,
            "{:<24} {:>10.2} {:>34} {:>10} KB",
            "Total", total_time_ms, "", peak_mem_kb,
        )?;
        writeln!(
            f,
            "{:<24} {:>36} {:>10} KB",
            "",
            "",
            format!("net: {total_mem_delta_kb:+}"),
        )?;
        Ok(())
    }
}
