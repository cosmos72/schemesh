{
  pkgs ? import <nixpkgs> { },
  schemesh ? pkgs.callPackage ./package.nix { },
}:

pkgs.mkShell {
  inputsFrom = [ schemesh ];

  packages = [
    pkgs.clang-tools
  ]
  ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ pkgs.gdb ];
}
