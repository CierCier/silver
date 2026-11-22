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
  CreateDirectoryW
  CreateFileW
  CreateProcessW
  CreateThread
  DeleteFileW
  ExitProcess
  ExitThread
  FindClose
  FindFirstFileW
  FindNextFileW
  FreeEnvironmentStringsW
  GetCommandLineW
  GetConsoleMode
  GetConsoleScreenBufferInfo
  GetCurrentDirectoryW
  GetCurrentProcess
  GetCurrentProcessId
  GetCurrentThreadId
  GetEnvironmentStringsW
  GetExitCodeProcess
  GetFileAttributesW
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
  RemoveDirectoryW
  SetConsoleCP
  SetConsoleMode
  SetConsoleOutputCP
  SetCurrentDirectoryW
  SetFilePointer
  Sleep
  SwitchToThread
  TerminateProcess
  VirtualAlloc
  VirtualFree
  WaitForSingleObject
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
# std.process reads the parent pid through NtQueryInformationProcess.
cat > ntdll.def <<'EOF'
LIBRARY ntdll.dll
EXPORTS
  NtQueryInformationProcess
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

# WaitOnAddress lives in the Synch API set on real Windows and is exported
# by kernelbase.dll on Windows 8+ and Wine; binding the import to kernelbase.dll
# keeps it resolvable under Wine as well.
cat > synchronization.def <<'EOF'
LIBRARY kernelbase.dll
EXPORTS
  WaitOnAddress
  WakeByAddressSingle
  WakeByAddressAll
EOF

# std.sys.os_windows emulates the Linux socket syscalls over winsock.
cat > ws2_32.def <<'EOF'
LIBRARY ws2_32.dll
EXPORTS
  WSAGetLastError
  WSAIoctl
  WSAPoll
  WSARecv
  WSASend
  WSASocketW
  WSAStartup
  accept
  bind
  closesocket
  connect
  getpeername
  getsockname
  getsockopt
  ioctlsocket
  listen
  recv
  recvfrom
  send
  sendto
  setsockopt
  shutdown
EOF

# std.net.dns discovers the system's DNS servers.
cat > iphlpapi.def <<'EOF'
LIBRARY iphlpapi.dll
EXPORTS
  GetNetworkParams
EOF

# `-lc` maps to legacy_stdio_definitions (the printf family as real
# symbols); here it binds the C entry points runtime_test calls to msvcrt.
cat > legacy_stdio_definitions.def <<'EOF'
LIBRARY msvcrt.dll
EXPORTS
  qsort
  sprintf
  strtod
EOF

# Rust staticlibs (silver_ffi) also name userenv and dbghelp; the default
# set lists them, so the link needs the (symbol-less here) libraries.
for name in userenv dbghelp; do
    printf 'LIBRARY %s.dll\nEXPORTS\n' "$name" > "$name.def"
done

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
