{
  description = "Workshop Preference Sorter";

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
        }
    );
}
