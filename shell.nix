let
  pkgs = import <nixpkgs> {};

in pkgs.mkShell {
  buildInputs = [
    # Haskell
    pkgs.stack
    pkgs.zlib
    pkgs.libiconv

    # JavaScript
    pkgs.nodejs-10_x
  ];
}