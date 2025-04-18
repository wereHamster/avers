let
  pkgs = import <nixpkgs> {};

in {
  canary = { n, commit }: pkgs.stdenv.mkDerivation {
    name = "avers";
    src = ./.;

    buildInputs = [
      pkgs.nodejs
      pkgs.jq
      pkgs.biome
    ];

    buildPhase = ''
      HOME=$PWD npm ci
      npm version patch
      ./node_modules/.bin/tsc
      ./node_modules/.bin/rollup -c rollup.config.js
      cat package.json | jq 'del(.devDependencies) | del(.ava) | .version = .version + "-alpha.${n}+${commit}"' > dist/package.json
    '';

    checkPhase = ''
      biome lint .
      ./node_modules/.bin/ava
    '';

    installPhase = ''
      mv dist $out
      rm $out/*.test.*
    '';
  };
}
