{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/cb4346597f033750a432f0161eb5d2c426776960";
  };
  outputs = { nixpkgs, self}:
  let
    system = "x86_64-linux"; 
    pkgs = import nixpkgs { inherit system; };
  in
  {
    legacyPackages."${system}" = pkgs;
    devShell."${system}" = pkgs.mkShell {
      buildInputs = with pkgs; [
        stack
        haskell.compiler.ghc822Binary
        stdenv.cc
        openssl
        zlib
        postgresql
        pkg-config
        # frontend dependencies
        nodejs
        python3
      ];

      LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [pkgs.zlib pkgs.openssl];
    };
  };
}
