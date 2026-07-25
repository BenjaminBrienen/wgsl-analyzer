use std::{
    collections::BTreeMap,
    fs,
    io::Write as _,
    path::Path,
    time::{Instant, SystemTime, UNIX_EPOCH},
};

use anyhow::format_err;
use xshell::{Shell, cmd};

use crate::flags::{self, MeasurementType};

type Unit = String;

impl flags::Metrics {
    pub(crate) fn run(self, sh: &Shell) -> anyhow::Result<()> {
        let mut metrics = Metrics::new(sh)?;
        let _env = sh.push_env("WA_METRICS", "1");

        let name = match &self.measurement_type {
            Some(ms) => {
                let name = ms.as_ref();
                match ms {
                    MeasurementType::Build => {
                        metrics.measure_build(sh)?;
                    }
                    MeasurementType::AnalyzeBevy20 => {
                        metrics.measure_analysis_stats_path(sh, name, "../bevy20")?;
                    }
                };
                name
            }
            None => {
                metrics.measure_build(sh)?;
                metrics.measure_analysis_stats_path(sh, MeasurementType::AnalyzeBevy20.as_ref(), "../bevy20")?;
                "all"
            }
        };

        let mut file =
            fs::File::options().write(true).create(true).open(format!("target/{name}.json"))?;
        writeln!(file, "{}", metrics.json())?;
        eprintln!("{metrics:#?}");
        Ok(())
    }
}

impl Metrics {
    fn measure_build(&mut self, sh: &Shell) -> anyhow::Result<()> {
        eprintln!("\nMeasuring build");
        cmd!(sh, "cargo clean").run()?;
        cmd!(sh, "cargo fetch").run()?;

        let time = Instant::now();
        cmd!(sh, "cargo build --release --package wgsl-analyzer --bin wgsl-analyzer").run()?;
        let time = time.elapsed();
        self.report("build", time.as_millis() as u64, "ms".into());
        Ok(())
    }

    fn measure_analysis_stats_path(
        &mut self,
        sh: &Shell,
        name: &str,
        path: &str,
    ) -> anyhow::Result<()> {
        assert!(Path::new(path).exists(), "unable to find bench in {path}");
        eprintln!("\nMeasuring analysis-stats/{name}");
        let output = cmd!(sh, "./target/release/wgsl-analyzer -q analysis-stats {path}").read()?;
        for (metric, value, unit) in parse_metrics(&output) {
            self.report(&format!("analysis-stats/{name}/{metric}"), value, unit.into());
        }
        Ok(())
    }
}

fn parse_metrics(output: &str) -> Vec<(&str, u64, &str)> {
    output
        .lines()
        .filter_map(|it| {
            let entry = it.split(':').collect::<Vec<_>>();
            match entry.as_slice() {
                ["METRIC", name, value, unit] => Some((*name, value.parse().unwrap(), *unit)),
                _ => None,
            }
        })
        .collect()
}

#[derive(Debug)]
struct Metrics {
    host: Host,
    timestamp: SystemTime,
    revision: String,
    metrics: BTreeMap<String, (u64, Unit)>,
}

#[derive(Debug)]
struct Host {
    os: String,
    cpu: String,
    mem: String,
}

impl Metrics {
    fn new(sh: &Shell) -> anyhow::Result<Metrics> {
        let host = Host::new(sh).unwrap_or_else(|_| Host::unknown());
        let timestamp = SystemTime::now();
        let revision = cmd!(sh, "git rev-parse HEAD").read()?;
        Ok(Metrics { host, timestamp, revision, metrics: BTreeMap::new() })
    }

    fn report(&mut self, name: &str, value: u64, unit: Unit) {
        self.metrics.insert(name.into(), (value, unit));
    }

    fn json(&self) -> String {
        let mut buf = String::new();
        self.to_json(write_json::object(&mut buf));
        buf
    }

    fn to_json(&self, mut obj: write_json::Object<'_>) {
        self.host.to_json(obj.object("host"));
        let timestamp = self.timestamp.duration_since(UNIX_EPOCH).unwrap();
        obj.number("timestamp", timestamp.as_secs() as f64);
        obj.string("revision", &self.revision);
        let mut metrics = obj.object("metrics");
        for (k, (value, unit)) in &self.metrics {
            metrics.array(k).number(*value as f64).string(unit);
        }
    }
}

impl Host {
    fn unknown() -> Host {
        Host { os: "unknown".into(), cpu: "unknown".into(), mem: "unknown".into() }
    }

    fn new(sh: &Shell) -> anyhow::Result<Host> {
        if cfg!(not(target_os = "linux")) {
            return Ok(Host::unknown());
        }

        let os = read_field(sh, "/etc/os-release", "PRETTY_NAME=")?.trim_matches('"').to_owned();

        let cpu = read_field(sh, "/proc/cpuinfo", "model name")?
            .trim_start_matches(':')
            .trim()
            .to_owned();

        let mem = read_field(sh, "/proc/meminfo", "MemTotal:")?;

        return Ok(Host { os, cpu, mem });

        fn read_field(sh: &Shell, path: &str, field: &str) -> anyhow::Result<String> {
            let text = sh.read_file(path)?;

            text.lines()
                .find_map(|it| it.strip_prefix(field))
                .map(|it| it.trim().to_owned())
                .ok_or_else(|| format_err!("can't parse {}", path))
        }
    }
    fn to_json(&self, mut obj: write_json::Object<'_>) {
        obj.string("os", &self.os).string("cpu", &self.cpu).string("mem", &self.mem);
    }
}
