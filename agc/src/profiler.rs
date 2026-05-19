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
}

impl PhaseRecord {
    pub fn memory_delta_kb(&self) -> i64 {
        self.memory_after_kb as i64 - self.memory_before_kb as i64
    }
}

#[derive(Debug, Default)]
pub struct Profiler {
    pub enabled: bool,
    pub records: Vec<PhaseRecord>,
    pub start_time: Option<Instant>,
    pub total_memory_before_kb: u64,
}

impl Profiler {
    pub fn new(enabled: bool) -> Self {
        Self {
            enabled,
            records: Vec::new(),
            start_time: None,
            total_memory_before_kb: 0,
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
        let _ = name;
    }

    pub fn end_phase(&mut self, name: &str) {
        if !self.enabled {
            return;
        }
        let elapsed = self.start_time.map(|t| t.elapsed()).unwrap_or_default();
        let elapsed_ms = elapsed.as_secs_f64() * 1000.0;
        let memory_before_kb = self.total_memory_before_kb;
        let memory_after_kb = read_rss_kb().unwrap_or(0);

        self.records.push(PhaseRecord {
            name: name.to_string(),
            elapsed_ms,
            memory_before_kb,
            memory_after_kb,
        });

        self.start_time = Some(Instant::now());
        self.total_memory_before_kb = memory_after_kb;
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

        eprintln!("\n{}", "=== Compiler Profile Report ===".bright_cyan().bold());
        eprintln!(
            "{:<24} {:>10} {:>12} {:>12} {:>12}",
            "Phase", "Time (ms)", "Mem Before", "Mem After", "Delta (KB)"
        );
        eprintln!("{}", "-".repeat(74).bright_black());

        for record in &self.records {
            let delta = record.memory_delta_kb();
            let delta_str = if delta >= 0 {
                format!("+{delta}")
            } else {
                format!("{delta}")
            };
            eprintln!(
                "{:<24} {:>10.2} {:>10} KB {:>10} KB {:>10} KB",
                record.name,
                record.elapsed_ms,
                record.memory_before_kb,
                record.memory_after_kb,
                delta_str,
            );
        }

        eprintln!("{}", "-".repeat(74).bright_black());
        eprintln!(
            "{:<24} {:>10.2} {:>34} {:>10} KB",
            "Total",
            total_time_ms,
            "",
            peak_mem_kb,
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

pub fn read_rss_kb() -> Option<u64> {
    let status = fs::read_to_string("/proc/self/status").ok()?;
    for line in status.lines() {
        if let Some(rest) = line.strip_prefix("VmRSS:") {
            return rest.trim().split_whitespace().next()?.parse().ok();
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
            let delta = record.memory_delta_kb();
            let delta_str = if delta >= 0 {
                format!("+{delta}")
            } else {
                format!("{delta}")
            };
            writeln!(
                f,
                "{:<24} {:>10.2} {:>10} KB {:>10} KB {:>10} KB",
                record.name,
                record.elapsed_ms,
                record.memory_before_kb,
                record.memory_after_kb,
                delta_str,
            )?;
        }

        writeln!(f, "{}", "-".repeat(74))?;
        writeln!(
            f,
            "{:<24} {:>10.2} {:>34} {:>10} KB",
            "Total",
            total_time_ms,
            "",
            peak_mem_kb,
        )?;
        writeln!(
            f,
            "{:<24} {:>36} {:>10} KB",
            "",
            "",
            format!("net: {}:+", total_time_ms),
        )?;
        Ok(())
    }
}
