{ pkgs, gitignore-nix, haskell, webCommon, webCommonSimeon, buildPursPackage, buildNodeModules, filterNpm, zerepoch-pab }:
let
  simeon-invoker = haskell.packages.simeon.components.exes.simeon-pab;

  generated-purescript = pkgs.runCommand "simeon-pab-purescript" { } ''
    mkdir $out
    ${zerepoch-pab.server-setup-invoker}/bin/zerepoch-pab-setup psgenerator $out
    ln -s ${./zerepoch-pab.yaml} zerepoch-pab.yaml
    ${simeon-invoker}/bin/simeon-pab --config zerepoch-pab.yaml psapigenerator $out
  '';

  generate-purescript = pkgs.writeShellScriptBin "simeon-pab-generate-purs" ''
    generatedDir=./generated
    rm -rf $generatedDir
    $(nix-build ../default.nix --quiet --no-build-output -A zerepoch-pab.server-setup-invoker)/bin/zerepoch-pab-setup psgenerator $generatedDir
    $(nix-build ../default.nix --quiet --no-build-output -A simeon-dashboard.simeon-invoker)/bin/simeon-pab --config zerepoch-pab.yaml psapigenerator $generatedDir
  '';

  start-backend = pkgs.writeShellScriptBin "simeon-pab-server" ''
    echo "simeon-pab-server: for development use only"
    $(nix-build ../default.nix --quiet --no-build-output -A simeon-dashboard.simeon-invoker)/bin/simeon-pab --config zerepoch-pab.yaml all-servers
  '';

  cleanSrc = gitignore-nix.gitignoreSource ./.;

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
    githubSourceHashMap = { };
  };

  client = buildPursPackage {
    inherit pkgs nodeModules;
    src = cleanSrc;
    checkPhase = ''
      node -e 'require("./output/Test.Main").main()'
    '';
    name = "simeon-dashboard-client";
    extraSrcs = {
      web-common = webCommon;
      web-common-simeon = webCommonSimeon;
      generated = generated-purescript;
    };
    packages = pkgs.callPackage ./packages.nix { };
    spagoPackages = pkgs.callPackage ./spago-packages.nix { };
  };
in
{
  inherit (zerepoch-pab) server-setup-invoker;
  inherit client simeon-invoker generate-purescript generated-purescript start-backend;
}
