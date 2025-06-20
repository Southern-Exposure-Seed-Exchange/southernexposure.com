{
  inputs = {
    nixpkgs-old.url = "github:nixos/nixpkgs/cb4346597f033750a432f0161eb5d2c426776960";
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { nixpkgs, nixpkgs-old, ...}:
  let
    system = "x86_64-linux"; 
    pkgs = import nixpkgs { inherit system; };
    pkgs-old = import nixpkgs-old { inherit system; };
  in
  {
    legacyPackages."${system}" = pkgs;
    devShell."${system}" = pkgs.mkShell {
      buildInputs = with pkgs; [
        stack
        haskell.compiler.ghc8107
        stdenv.cc
        openssl
        zlib
        postgresql
        postgresql.pg_config
        pkg-config
        # frontend dependencies
        pkgs-old.nodejs
        pkgs-old.python3
      ];

      LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [pkgs.zlib pkgs.openssl];
    };
  };
}
