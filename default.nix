{ pkgs ? import <nixpkgs> {} }:
let
  versionLatest =
    builtins.head
      (builtins.match ".*Schemesh Version ([0-9.]+).*"
        (builtins.readFile ./bootstrap/functions.ss));
in
  pkgs.stdenv.mkDerivation {
    name = "schemesh";
    version = versionLatest;
    src = ./.;

    buildInputs = [
      pkgs.chez    # Ubuntu: chezscheme-dev
      pkgs.lz4     # Ubuntu: liblz4-dev
      pkgs.ncurses # Ubuntu: libncurses-dev
      pkgs.libuuid # Ubuntu: uuid-dev
      pkgs.zlib    # Ubuntu: zlib1g-dev
    ];

    nativeBuildInputs = [
      pkgs.patchelf
    ];

    buildPhase = ''
      make -j prefix=$out
    '';

    installPhase = ''
      mkdir -p $out/bin $out/lib/schemesh

      cp schemesh $out/bin/schemesh
      cp "libschemesh_${versionLatest}.so" $out/lib/schemesh/
      chmod +x $out/bin/schemesh

      patchelf $out/bin/schemesh --set-rpath \
        "${pkgs.lib.makeLibraryPath [ pkgs.ncurses pkgs.libuuid ]}"
    '';
  }
