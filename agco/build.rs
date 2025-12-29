use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=.git/HEAD");

    let output = Command::new("git")
        .args(&["describe", "--tags", "--always", "--dirty"])
        .output();

    let git_desc = match output {
        Ok(o) if o.status.success() => String::from_utf8(o.stdout).unwrap_or_default(),
        _ => "unknown".to_string(),
    };
    
    let clean_desc = git_desc.trim();

    println!("cargo:rustc-env=SILVER_GIT_DESCRIBE={}", clean_desc);
}
