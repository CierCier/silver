#!/usr/bin/env bash
# Generate COFF import libraries for the freestanding Windows link, so the
# test harness can cross-compile and run Silver PEs from a posix host:
#
#   scripts/gen-win-importlibs.sh /tmp/wimplib
#   python3 tests/run_tests.py --target x86_64-pc-windows-msvc \
#       --libdir /tmp/wimplib --runner wine <filters...>
#
# Requires: llvm-dlltool (LLVM 22 binutils), matching the compiler's LLVM.
# The CRT libs (msvcrt/ucrt/vcruntime) are emitted as import libs with no
# symbols: the runtime is freestanding (custom _start, no CRT), the entries
# only satisfy the driver's default library set.
set -euo pipefail

OUT="${1:?usage: gen-win-importlibs.sh <output-dir>}"
DLLTOOL="${DLLTOOL:-llvm-dlltool}"
if ! command -v "$DLLTOOL" >/dev/null 2>&1; then
    for candidate in /usr/lib/llvm-22/bin/llvm-dlltool /usr/bin/llvm-dlltool-22; do
        if [ -x "$candidate" ]; then DLLTOOL="$candidate"; break; fi
    done
fi

mkdir -p "$OUT"
cd "$OUT"

cat > kernel32.def <<'EOF'
LIBRARY kernel32.dll
EXPORTS
  CloseHandle
  CreateFileW
  CreateThread
  DeleteFileW
  ExitProcess
  ExitThread
  GetCurrentProcess
  GetCurrentProcessId
  GetCommandLineW
  GetFileInformationByHandle
  GetLastError
  GetStdHandle
  GetSystemTimePreciseAsFileTime
  LocalFree
  MoveFileExW
  OpenProcess
  QueryPerformanceCounter
  QueryPerformanceFrequency
  ReadFile
  SetConsoleCP
  SetConsoleOutputCP
  SetFilePointer
  Sleep
  SwitchToThread
  TerminateProcess
  VirtualAlloc
  VirtualFree
  WaitOnAddress
  WakeByAddressAll
  WakeByAddressSingle
  WriteFile
EOF

cat > shell32.def <<'EOF'
LIBRARY shell32.dll
EXPORTS
  CommandLineToArgvW
EOF

# The OS unwinder + leak-check attribution live in ntdll (backtrace.ag,
# mem/alloc.ag). RtlCaptureStackBackTrace is also forwarded by kernel32,
# but the unwind trio is ntdll-only, so bind all four to ntdll.dll.
cat > ntdll.def <<'EOF'
LIBRARY ntdll.dll
EXPORTS
  RtlCaptureContext
  RtlCaptureStackBackTrace
  RtlLookupFunctionEntry
  RtlVirtualUnwind
EOF

cat > bcrypt.def <<'EOF'
LIBRARY bcrypt.dll
EXPORTS
  BCryptGenRandom
EOF

# WaitOnAddress lives in the Synch API set on real Windows; binding the
# import to kernel32.dll keeps it resolvable under Wine as well.
cat > synchronization.def <<'EOF'
LIBRARY kernel32.dll
EXPORTS
  WaitOnAddress
  WakeByAddressSingle
  WakeByAddressAll
EOF

for name in msvcrt ucrt vcruntime; do
    case "$name" in
        msvcrt)    image="msvcrt.dll" ;;
        ucrt)      image="ucrtbase.dll" ;;
        vcruntime) image="vcruntime140.dll" ;;
    esac
    printf 'LIBRARY %s\nEXPORTS\n' "$image" > "$name.def"
done

for def in *.def; do
    lib="${def%.def}.lib"
    "$DLLTOOL" -m i386:x86-64 -d "$def" -l "$lib"
done

echo "import libraries written to $OUT:"
ls -1 *.lib
