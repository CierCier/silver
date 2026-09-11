$ErrorActionPreference = "Stop"

# 1. Build-time: llvm-sys finds the llvm-config shim here.
[Environment]::SetEnvironmentVariable(
    "LLVM_SYS_221_PREFIX",
    "$env:LOCALAPPDATA\silver\llvm-shim",
    "User")

# 2. Run-time: agc.exe resolves LLVM-C.dll (and lld-link/clang) via PATH.
$llvmBin = "C:\Program Files\LLVM\bin"
$userPath = [Environment]::GetEnvironmentVariable("Path", "User")
if ($userPath -notlike "*$llvmBin*") {
    [Environment]::SetEnvironmentVariable("Path", "$userPath;$llvmBin", "User")
    Write-Host "PATH updated (User scope): + $llvmBin"
} else {
    Write-Host "PATH already contains $llvmBin"
}

Write-Host "LLVM_SYS_221_PREFIX = $([Environment]::GetEnvironmentVariable('LLVM_SYS_221_PREFIX','User'))"
