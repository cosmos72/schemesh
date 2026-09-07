# Compatibility entry point for `nix-build` (flakes not required).
# The package itself is defined in ./package.nix, which flake.nix also uses.
{
  pkgs ? import <nixpkgs> { },
}:

pkgs.callPackage ./package.nix { }
