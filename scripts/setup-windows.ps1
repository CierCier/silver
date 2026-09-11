# Silver Windows toolchain bootstrap.
#
# Verifies and (optionally, with -InstallMissing) installs everything needed to
# build and run the Silver compiler on Windows x64:
#   1. LLVM 22.x (official winget install — shared-only: LLVM-C.dll + LLVM-C.lib)
#   2. VS 2022 Build Tools w/ C++ workload + Windows SDK (rustc linker, CRT libs)
#   3. Rust toolchain (x86_64-pc-windows-msvc host)
#   4. The llvm-config shim (official LLVM has no llvm-config.exe and no static
#      libs; llvm-sys needs both quirks papered over — see docs/windows-port.md
#      section 2)
#
# Usage:
#   powershell -ExecutionPolicy Bypass -File scripts\setup-windows.ps1
#   powershell -ExecutionPolicy Bypass -File scripts\setup-windows.ps1 -InstallMissing
#
# Env vars set persistently (User scope):
#   LLVM_SYS_221_PREFIX -> %LOCALAPPDATA%\silver\llvm-shim
#   PATH += C:\Program Files\LLVM\bin   (LLVM-C.dll must resolve at agc runtime)

param(
    [switch]$InstallMissing
)

$ErrorActionPreference = "Stop"
$RepoRoot = Split-Path -Parent $PSScriptRoot
$LlvmRoot = if ($env:SILVER_LLVM_ROOT) { $env:SILVER_LLVM_ROOT } else { "C:\Program Files\LLVM" }
$ShimPrefix = Join-Path $env:LOCALAPPDATA "silver\llvm-shim"
$Failures = @()

function Step($msg) { Write-Host "==> $msg" -ForegroundColor Cyan }
function Ok($msg)   { Write-Host "    OK  $msg" -ForegroundColor Green }
function Warn($msg) { Write-Host "    !!  $msg" -ForegroundColor Yellow }

Step "Checking winget"
if (-not (Get-Command winget -ErrorAction SilentlyContinue)) {
    throw "winget not found. Install 'App Installer' from the Microsoft Store, then re-run."
}
Ok "winget $(winget --version)"

# --- 1. LLVM ---------------------------------------------------------------
Step "Checking LLVM at $LlvmRoot"
$LlvmClang = Join-Path $LlvmRoot "bin\clang.exe"
if (-not (Test-Path $LlvmClang)) {
    if ($InstallMissing) {
        Write-Host "    installing LLVM via winget (this downloads ~400 MB)..."
        winget install --id LLVM.LLVM -e --accept-package-agreements --accept-source-agreements
        if ($LASTEXITCODE -ne 0) { throw "LLVM install failed" }
    } else {
        $Failures += "LLVM 22.x not found at $LlvmRoot (re-run with -InstallMissing, or 'winget install LLVM.LLVM')"
    }
} else {
    Ok "clang present"
}
if (Test-Path $LlvmClang) {
    $llvmVer = & $LlvmClang --version | Select-Object -First 1
    Ok $llvmVer
    if ($llvmVer -notmatch "version (\d+)\.") {
        throw "could not parse LLVM version"
    }
    $llvmMajor = $Matches[1]
    if ($llvmMajor -lt 22) { Warn "LLVM $llvmMajor is older than the required 22.x (llvm-sys 221)" }
    if (Test-Path (Join-Path $LlvmRoot "lib\LLVM-C.lib")) {
        Ok "LLVM-C.lib (import lib) present"
    } else {
        $Failures += "LLVM-C.lib missing from $LlvmRoot\lib — install the official LLVM package"
    }
}

# --- 2. Visual Studio C++ toolchain + Windows SDK --------------------------
Step "Checking Visual Studio C++ Build Tools"
$vswhere = "${env:ProgramFiles(x86)}\Microsoft Visual Studio\Installer\vswhere.exe"
$vsRoot = $null
if (Test-Path $vswhere) {
    $vsRoot = & $vswhere -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath | Select-Object -First 1
}
if ($vsRoot) {
    Ok "VS with C++ tools: $vsRoot"
} elseif ($InstallMissing) {
    Write-Host "    installing VS 2022 Build Tools with the C++ workload (multi-GB, takes a while)..."
    winget install --id Microsoft.VisualStudio.2022.BuildTools -e `
        --override "--quiet --wait --add Microsoft.VisualStudio.Workload.VCTools --includeRecommended" `
        --accept-package-agreements --accept-source-agreements
    if ($LASTEXITCODE -ne 0) { throw "VS Build Tools install failed" }
    Ok "installed VS 2022 Build Tools"
} else {
    $Failures += "VS 2022 C++ workload not found (re-run with -InstallMissing, or install 'Microsoft.VisualStudio.2022.BuildTools' with the VCTools workload)"
}

$sdkLib = Get-ChildItem "${env:ProgramFiles(x86)}\Windows Kits\10\Lib" -Directory -ErrorAction SilentlyContinue |
    Sort-Object Name -Descending | Select-Object -First 1
if ($sdkLib) {
    Ok "Windows SDK $($sdkLib.Name)"
} else {
    $Failures += "Windows SDK not found (comes with the VS C++ workload)"
}

# --- 3. Rust (MSVC host) ---------------------------------------------------
Step "Checking Rust"
if (Get-Command rustc -ErrorAction SilentlyContinue) {
    $hostTriple = rustc -vV | Select-String "^host:"
    Ok "rustc $(rustc --version) [$($hostTriple -replace 'host: ','')]"
    if ($hostTriple -notmatch "windows-msvc") {
        Warn "Rust host is not windows-msvc; the MSVC CRT link path assumes it"
    }
} else {
    $Failures += "Rust not found (install via https://rustup.rs, default MSVC toolchain)"
}

# --- 4. llvm-config shim ---------------------------------------------------
Step "Building llvm-config shim"
if (-not (Get-Command cargo -ErrorAction SilentlyContinue)) {
    $Failures += "cargo not found; cannot build the llvm-config shim"
} else {
    Push-Location (Join-Path $RepoRoot "scripts\llvm-config-shim")
    try {
        cargo build --release
        if ($LASTEXITCODE -ne 0) { throw "shim build failed" }
    } finally { Pop-Location }
    $shimBin = Join-Path $ShimPrefix "bin"
    New-Item -ItemType Directory -Force -Path $shimBin | Out-Null
    Copy-Item (Join-Path $RepoRoot "scripts\llvm-config-shim\target\release\llvm-config.exe") `
        (Join-Path $shimBin "llvm-config.exe") -Force
    Ok "shim deployed to $shimBin\llvm-config.exe"

    Step "Setting persistent environment (User scope)"
    [Environment]::SetEnvironmentVariable("LLVM_SYS_221_PREFIX", $ShimPrefix, "User")
    Ok "LLVM_SYS_221_PREFIX=$ShimPrefix"

    $llvmBin = Join-Path $LlvmRoot "bin"
    $userPath = [Environment]::GetEnvironmentVariable("Path", "User")
    $machinePath = [Environment]::GetEnvironmentVariable("Path", "Machine")
    if (($userPath -notlike "*$llvmBin*") -and ($machinePath -notlike "*$llvmBin*")) {
        [Environment]::SetEnvironmentVariable("Path", "$userPath;$llvmBin", "User")
        Ok "added $llvmBin to user PATH (LLVM-C.dll resolution + lld-link/clang)"
    } else {
        Ok "$llvmBin already on PATH"
    }

    $env:LLVM_SYS_221_PREFIX = $ShimPrefix
    if ($llvmBin -notin ($env:Path -split ";")) { $env:Path += ";$llvmBin" }

    Step "Smoke-testing shim"
    $v = & (Join-Path $shimBin "llvm-config.exe") --version
    Ok "llvm-config --version -> $v"
}

# --- 5. Compiler build -----------------------------------------------------
if (-not $Failures) {
    Step "Building agc (cargo build -p agc)"
    Push-Location $RepoRoot
    try {
        cargo build -p agc
        if ($LASTEXITCODE -ne 0) { throw "agc build failed" }
    } finally { Pop-Location }
    Ok "agc built — Windows compiler toolchain is complete"
} else {
    Write-Host ""
    Warn "Missing components (fix these, or re-run with -InstallMissing):"
    $Failures | ForEach-Object { Write-Host "  - $_" -ForegroundColor Yellow }
    exit 1
}
