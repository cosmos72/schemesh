{
  lib,
  stdenv,
  chez,
  lz4,
  ncurses,
  zlib,
  libuuid,
  libiconv,
}:

let
  # Single source of truth for the version: bootstrap/functions.ss
  version = builtins.head (
    builtins.match ".*Schemesh Version ([0-9.]+).*" (builtins.readFile ./bootstrap/functions.ss)
  );
in
stdenv.mkDerivation {
  pname = "schemesh";
  inherit version;

  # The whole working tree, minus VCS metadata. Deliberately *not* pruned any
  # further: test/data3.ss glob-expands the source root and asserts on the
  # result, so the build tree has to match what a developer sees in a checkout.
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.difference ./. (lib.fileset.maybeMissing ./.git);
  };

  strictDeps = true;

  # `scheme` is needed on $PATH at build time: utils/find_chez_scheme_dir.sh runs it
  # to locate the boot/kernel directory that the Makefile compiles and links against.
  nativeBuildInputs = [ chez ];

  buildInputs = [
    chez
    lz4
    ncurses
    zlib
  ]
  ++ lib.optionals stdenv.hostPlatform.isLinux [ libuuid ]
  ++ lib.optionals stdenv.hostPlatform.isDarwin [ libiconv ];

  makeFlags = [
    "CC=${stdenv.cc.targetPrefix}cc"
    "prefix=${placeholder "out"}"
  ]
  # The Makefile defaults to `LDFLAGS=-s`, which Apple's ld64 no longer accepts.
  # Nix strips binaries in a separate phase anyway.
  ++ lib.optionals stdenv.hostPlatform.isDarwin [ "LDFLAGS=" ];

  enableParallelBuilding = true;

  # lib/schemesh/libschemesh_$VERSION.so is a Chez Scheme fasl object, not a native
  # shared library. The default fixupPhase strip truncates it to 24 bytes, and the
  # resulting binary dies with "incompatible fasl-object version". Only strip bin/.
  stripDebugList = [ "bin" ];

  # `make` builds schemesh_test and runs it to produce libschemesh_$VERSION.so,
  # so the test suite is already exercised during buildPhase.
  doCheck = false;

  meta = {
    description = "Fusion between a Unix shell and a Lisp REPL";
    homepage = "https://github.com/cosmos72/schemesh";
    license = lib.licenses.gpl2Plus;
    mainProgram = "schemesh";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
}
