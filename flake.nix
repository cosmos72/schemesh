{
  description = "Schemesh - A Unix shell and Lisp REPL, fused together";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ self, flake-parts, ... }:

    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
        # "aarch64-linux" # not tested
        # "x86_64-darwin" # not tested
      ];

      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          sharedBuildInputs = [
            pkgs.chez    # Ubuntu: chezscheme-dev
            pkgs.lz4     # Ubuntu: liblz4-dev
            pkgs.ncurses # Ubuntu: libncurses-dev
            pkgs.git     # Ubuntu: git
            pkgs.libuuid # Ubuntu: uuid-dev
            pkgs.zlib    # Ubuntu: zlib1g-dev
            pkgs.makeWrapper
          ];
        in
          {
            packages = {
              default = pkgs.stdenv.mkDerivation rec {
                name = "schemesh";
                version = "0.8.1";
                src = self;

                buildInputs = sharedBuildInputs;

                buildPhase = ''
                  make -j prefix=$out
                '';

                installPhase = ''
                  mkdir -p $out/bin $out/lib/schemesh

                  cp schemesh $out/bin/schemesh.unwrapped
                  chmod +x $out/bin/schemesh.unwrapped

                  cp libschemesh_${version}.so $out/lib/schemesh/
                  ${pkgs.patchelf}/bin/patchelf --set-rpath "$out/lib/schemesh" $out/bin/schemesh.unwrapped

                  makeWrapper $out/bin/schemesh.unwrapped $out/bin/schemesh \
                    --prefix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath [ pkgs.ncurses pkgs.libuuid ]}:$out/lib/"
                '';
              };
            };

            apps = {
              default = {
                type = "app";
                program = "${self'.packages.default}/bin/schemesh";
              };
            };

            devShells = {
              default = pkgs.mkShell {
                buildInputs = sharedBuildInputs;
              };
            };
          };
        };
    }

