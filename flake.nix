{
  description = "My personal website";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, nixpkgs }: 
    flake-utils.lib.eachDefaultSystem (system:
        let pkgs = import nixpkgs { inherit system; };
            native-build-inputs = with pkgs; [
                haskell.compiler.ghc910
                haskell.packages.ghc910.haskell-language-server
                haskellPackages.cabal-install
                haskellPackages.hakyll
                zlib
            ];
        in rec {
            devShell = pkgs.mkShell {
                nativeBuildInputs = native-build-inputs;
            };

            defaultPackage = pkgs.haskellPackages.callCabal2nix "adam-portfolio" ./. {
                pandoc = pkgs.haskellPackages.callHackage "pandoc" "3.8" {
                    citeproc = pkgs.haskellPackages.callHackage "citeproc" "0.10" { };
                    texmath = pkgs.haskellPackages.callHackage "texmath" "0.13" { };
                };
            };

            packages.default = pkgs.haskell.lib.addBuildTools defaultPackage (with pkgs.haskellPackages; [
                alex
                happy
                cpphs
                hscolour
                uhc-light
                pkgs.pkg-config
            ]);
        }
    );
}
