let
  pkgs = import <nixpkgs> {};

in {
  canary = { n, commit }: pkgs.stdenv.mkDerivation {
    name = "avers";
    src = ./.;

    buildInputs = [
      pkgs.nodejs
      pkgs.pnpm
      pkgs.jq
      pkgs.biome
    ];

    buildPhase = ''
      HOME=$PWD pnpm install --frozen-lockfile
      npm version patch
      ./node_modules/.bin/tsc
      jq 'del(.devDependencies) | .version = .version + "-alpha.${n}+${commit}"' package.json > temp.json && mv temp.json package.json
    '';

    checkPhase = ''
      biome lint .
      ./node_modules/.bin/ava
    '';

    installPhase = ''
      mv . $out
    '';
  };
}
