let
  pkgs = import <nixpkgs> {};

in {
  canary = { n, commit }: pkgs.stdenv.mkDerivation {
    name = "avers";
    src = ./.;

    buildInputs = [
      pkgs.nodejs
      pkgs.jq
    ];

    buildPhase = ''
      npm version patch
      jq 'del(.devDependencies) | .version = .version + "-alpha.${n}+${commit}"' package.json > temp.json && mv temp.json package.json
    '';

    installPhase = ''
      mkdir -p $out
      cp -rv . $out
      rm -rf $out/node_modules
    '';
  };
}
