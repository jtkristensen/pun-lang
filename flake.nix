{
  description = "Development setup for compiling Haskell using Cabal";

  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixos-23.11" ;
    flake-utils.url = "github:numtide/flake-utils"       ;
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        manager  = import nixpkgs { inherit system; };
        stack    = manager.haskellPackages.stack;
        terminal = manager.mkShell;
      in
      {
        devShell = terminal {
          buildInputs = [ stack ];
        };
      }
    );
}
