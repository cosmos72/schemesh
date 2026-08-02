{
  description = "A Unix shell and Lisp REPL, fused together";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      packages = forAllSystems (pkgs: rec {
        schemesh = pkgs.callPackage ./package.nix { };
        default = schemesh;
      });

      devShells = forAllSystems (pkgs: {
        default = import ./shell.nix {
          inherit pkgs;
          schemesh = self.packages.${pkgs.stdenv.hostPlatform.system}.schemesh;
        };
      });

      overlays.default = final: prev: {
        schemesh = final.callPackage ./package.nix { };
      };

      formatter = forAllSystems (pkgs: pkgs.nixfmt);
    };
}
