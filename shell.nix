let
  pkgs = import <nixpkgs> {};

in pkgs.mkShell {
  buildInputs = [
    # Haskell
    pkgs.stack
    pkgs.zlib
    pkgs.libiconv

    # JavaScript
    pkgs.nodejs
    pkgs.pnpm

    pkgs.biome
  ];
}
