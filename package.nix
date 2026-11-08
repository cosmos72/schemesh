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

stdenv.mkDerivation {
  pname = "schemesh";
  version = builtins.head (
    builtins.match ".*Schemesh Version ([0-9.]+).*" (builtins.readFile ./bootstrap/functions.ss)
  );

  # exclude build artifacts from `make` when not building as a flake
  src =
    if builtins.pathExists ./.git then
      lib.fileset.toSource {
        root = ./.;
        fileset = lib.fileset.gitTracked ./.;
      }
    else
      ./.;

  strictDeps = true;

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
  # Apple's ld64 rejects -s
  ++ lib.optionals stdenv.hostPlatform.isDarwin [ "LDFLAGS=" ];

  enableParallelBuilding = true;

  # libschemesh_*.so is a Chez Scheme fasl object, stripping it breaks it
  stripDebugList = [ "bin" ];

  meta = {
    description = "Fusion between a Unix shell and a Lisp REPL";
    homepage = "https://github.com/cosmos72/schemesh";
    license = lib.licenses.gpl2Plus;
    mainProgram = "schemesh";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
}
