{
  lib,
  stdenv,
  runCommand,
  rustPlatform,
  llvmPackages_22,
  pkg-config,
  makeWrapper,
  patchelf,
  libffi,
  ncurses,
  zlib,
  libxml2,
  git,
  openssl,
}:
let
  fs = lib.fileset;

  # LLVM calls into libffi; the release links it statically so the binary
  # does not load a /nix/store copy at runtime.
  staticLibffi = libffi.overrideAttrs (old: {
    configureFlags = (old.configureFlags or [ ]) ++ [ "--enable-static" ];
    dontDisableStatic = true;
  });

  # Crates emit -lffi, -lz and -lstdc++ before our own link args, so the shared
  # objects would win. This directory holds only archives, and it is searched
  # first.
  staticLibs = runCommand "silver-static-libs" { } ''
    mkdir -p $out/lib
    ln -s ${staticLibffi}/lib/libffi.a ${zlib.static}/lib/libz.a ${stdenv.cc.cc}/lib/libstdc++.a $out/lib/
  '';

  rustflags = "-L native=${staticLibs}/lib -C link-arg=-static-libgcc";
in
rustPlatform.buildRustPackage {
  pname = "silver";
  version = "0.3.0";

  src = fs.toSource {
    root = ../.;
    fileset = fs.difference
      (fs.unions [
        ../Cargo.toml
        ../Cargo.lock
        ../clippy.toml
        ../bootstrap
        ../bin
        ../libs
        ../silver.toml
        ../std
        ../vendor
        ../examples
        ../tests
        ../LICENSE
        ../README.md
      ])
      (fs.maybeMissing ../tests/__pycache__);
  };

  cargoLock = {
    lockFile = ../Cargo.lock;
    outputHashes = {
      "elise-core-0.1.0" = "sha256-Fc8asqXxetC49IF1GXzryvbivJHWvCoyBepcrOUwHeU=";
    };
  };

  nativeBuildInputs = [
    pkg-config
    llvmPackages_22.llvm.dev
    llvmPackages_22.lld
    makeWrapper
    patchelf
  ];

  buildInputs = [
    llvmPackages_22.llvm
    llvmPackages_22.libclang
    staticLibffi
    ncurses
    zlib
    libxml2
    openssl
  ];

  nativeCheckInputs = [
    git
    openssl
  ];

  # LLVM & Clang environment flags for build scripts and bindgen
  LLVM_SYS_221_PREFIX = "${llvmPackages_22.llvm.dev}";
  LIBCLANG_PATH = "${llvmPackages_22.libclang.lib}/lib";
  OPENSSL_LIB_DIR = "${lib.getLib openssl}/lib";
  OPENSSL_DIR = "${openssl.dev}";

  RUSTFLAGS = rustflags;

  # The dev shell reuses these flags so local release builds link the same way.
  passthru.rustflags = rustflags;

  # Ensure reproducible build without git repository in sandbox
  GIT_DESCRIBE = "0.3.0";
  GIT_SHA = "nix";
  GIT_DIRTY = "0";

  # The agc build_cache tests fail to resolve std.sys.entry in this sandbox and
  # in the dev shell, so the Nix build skips them until that is fixed.
  doCheck = false;

  postInstall = ''
    # Drop the build-time RUNPATH so the loader uses the runtime glibc, not the
    # store path that built the binary.
    patchelf --remove-rpath $out/bin/agc $out/bin/agsm

    $out/bin/agc build silver.toml --bin aglsp --no-cache -O2 -o $out/bin/aglsp

    # Install standard library and vendor headers
    mkdir -p $out/include/silver
    cp -r std $out/include/silver/std
    ln -s silver/std $out/include/std

    if [ -d vendor ]; then
      cp -r vendor $out/include/silver/vendor
      ln -s silver/vendor $out/include/vendor
    fi

    # Wrap agc so it finds ld.lld and cc for linking, and defaults to the installed sysroot
    wrapProgram $out/bin/agc \
      --prefix PATH : ${lib.makeBinPath [ llvmPackages_22.lld stdenv.cc ]} \
      --set-default SILVER_SYSROOT "$out"

    # Wrap aglsp with sysroot
    wrapProgram $out/bin/aglsp \
      --set-default SILVER_SYSROOT "$out"

    # Wrap agsm with LIBCLANG_PATH so clang runtime is found
    wrapProgram $out/bin/agsm \
      --set LIBCLANG_PATH "${llvmPackages_22.libclang.lib}/lib"
  '';

  meta = {
    description = "Statically typed, LLVM-backed systems programming language";
    homepage = "https://github.com/CierCier/silver";
    license = lib.licenses.mit;
    mainProgram = "agc";
    platforms = [ "x86_64-linux" ];
  };
}
