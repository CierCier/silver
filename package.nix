{
  lib,
  stdenv,
  rustPlatform,
  llvmPackages_22,
  pkg-config,
  makeWrapper,
  libffi,
  ncurses,
  zlib,
  libxml2,
  git,
  openssl,
}:
let
  fs = lib.fileset;
in
rustPlatform.buildRustPackage {
  pname = "silver";
  version = "0.2.6";

  src = fs.toSource {
    root = ./.;
    fileset = fs.difference
      (fs.unions [
        ./Cargo.toml
        ./Cargo.lock
        ./clippy.toml
        ./bin
        ./bootstrap
        ./ffi
        ./std
        ./vendor
        ./examples
        ./tests
        ./LICENSE
        ./README.md
      ])
      (fs.maybeMissing ./tests/__pycache__);
  };

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "elise-core-0.1.0" = "sha256-Fc8asqXxetC49IF1GXzryvbivJHWvCoyBepcrOUwHeU=";
    };
  };

  nativeBuildInputs = [
    pkg-config
    llvmPackages_22.llvm.dev
    makeWrapper
  ];

  buildInputs = [
    llvmPackages_22.llvm
    llvmPackages_22.libclang
    libffi
    ncurses
    zlib
    libxml2
    openssl
  ];

  nativeCheckInputs = [
    git
    llvmPackages_22.lld
    openssl
  ];

  # LLVM & Clang environment flags for build scripts and bindgen
  LLVM_SYS_221_PREFIX = "${llvmPackages_22.llvm.dev}";
  LIBCLANG_PATH = "${llvmPackages_22.libclang.lib}/lib";
  OPENSSL_LIB_DIR = "${lib.getLib openssl}/lib";
  OPENSSL_DIR = "${openssl.dev}";

  # Ensure reproducible build without git repository in sandbox
  GIT_DESCRIBE = "0.2.6";
  GIT_SHA = "nix";
  GIT_DIRTY = "0";

  preCheck = ''
    export AGC="$(find "$PWD/target" -type f -name agc -perm -111 | head -n 1)"
  '';

  postInstall = ''
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
    platforms = lib.platforms.unix;
  };
}
