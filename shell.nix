# Development shell, shared by `nix-shell` and `nix develop`.
{
  pkgs ? import <nixpkgs> { },
  schemesh ? pkgs.callPackage ./package.nix { },
}:

pkgs.mkShell {
  # Pull in chez, lz4, ncurses, zlib and the platform-specific libs.
  inputsFrom = [ schemesh ];

  packages = [
    pkgs.clang-tools # clang-format, honours ./.clang-format
  ]
  ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.gdb ];

  shellHook = ''
    echo "schemesh dev shell"
    echo "  build: make -j"
    echo "  test:  ./schemesh_test"
  '';
}
