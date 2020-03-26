{ pkgs ? import <nixpkgs> {} }:
with pkgs;
with haskell;
  pkgs.mkShell {
    buildInputs = [ gcc stack gmp  ];

    
}

