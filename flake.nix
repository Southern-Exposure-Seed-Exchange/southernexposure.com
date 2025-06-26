{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
  };

  outputs = { nixpkgs, ...}:
  let
    system = "x86_64-linux"; 
    pkgs = import nixpkgs { 
      inherit system; 
    };
    ghc = "ghc8107";
    hp = pkgs.haskell.packages."${ghc}";
  in
  {
    legacyPackages."${system}" = pkgs;
    devShell."${system}" = pkgs.mkShell {
      buildInputs = with pkgs; [
        stack
        haskell.compiler."${ghc}"
        hp.haskell-language-server
        stdenv.cc
        openssl
        zlib
        postgresql
        pkg-config
        # frontend dependencies
        nodejs
        python3
        autoconf
        automake
        elmPackages.elm
        elmPackages.elm-language-server
      ];
      CXXFLAGS = "-std=c++17";
    };
  };
}
