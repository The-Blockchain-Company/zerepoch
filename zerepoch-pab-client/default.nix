{ pkgs, gitignore-nix, haskell, webCommon, webCommonZerepoch, buildPursPackage, buildNodeModules, filterNpm }:
let
  server-setup-invoker = haskell.packages.zerepoch-pab.components.exes.zerepoch-pab-setup;
  server-examples-invoker = haskell.packages.zerepoch-pab.components.exes.zerepoch-pab-examples;
  test-generator = haskell.packages.zerepoch-pab.components.exes.zerepoch-pab-test-psgenerator;

  generated-purescript = pkgs.runCommand "zerepoch-pab-purescript" { } ''
    mkdir $out
    ln -s ${haskell.packages.zerepoch-pab.src}/zerepoch-pab.yaml.sample zerepoch-pab.yaml
    ${server-setup-invoker}/bin/zerepoch-pab-setup psgenerator $out
    ${server-examples-invoker}/bin/zerepoch-pab-examples --config zerepoch-pab.yaml psapigenerator $out
    ${test-generator}/bin/zerepoch-pab-test-psgenerator $out
  '';

  # For dev usage
  generate-purescript = pkgs.writeShellScriptBin "zerepoch-pab-generate-purs" ''
    generatedDir=./generated
    rm -rf $generatedDir
    # There might be local modifications so only copy when missing
    ! test -f ./zerepoch-pab.yaml && cp ../zerepoch-pab/zerepoch-pab.yaml.sample zerepoch-pab.yaml
    $(nix-build ../default.nix --quiet --no-build-output -A zerepoch-pab.server-setup-invoker)/bin/zerepoch-pab-setup psgenerator $generatedDir
    $(nix-build ../default.nix --quiet --no-build-output -A zerepoch-pab.test-generator)/bin/zerepoch-pab-test-psgenerator $generatedDir
    $(nix-build ../default.nix --quiet --no-build-output -A zerepoch-pab.server-examples-invoker)/bin/zerepoch-pab-examples --config zerepoch-pab.yaml psapigenerator $generatedDir
  '';

  # For dev usage
  migrate = pkgs.writeShellScriptBin "zerepoch-pab-migrate" ''
    # There might be local modifications so only copy when missing
    ! test -f ./zerepoch-pab.yaml && cp ../zerepoch-pab/zerepoch-pab.yaml.sample zerepoch-pab.yaml
    $(nix-build ../default.nix --quiet --no-build-output -A zerepoch-pab.server-examples-invoker)/bin/zerepoch-pab-examples --config=zerepoch-pab.yaml migrate
  '';

  # For dev usage
  start-backend = pkgs.writeShellScriptBin "zerepoch-pab-server" ''
    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    # There might be local modifications so only copy when missing
    ! test -f ./zerepoch-pab.yaml && cp ../zerepoch-pab/zerepoch-pab.yaml.sample zerepoch-pab.yaml
    # Only execute the migration when the database file does not exist
    ! test -f ./$(yq -r '.dbConfig.dbConfigFile' zerepoch-pab.yaml) && zerepoch-pab-migrate
    $(nix-build ../default.nix --quiet --no-build-output -A zerepoch-pab.server-examples-invoker)/bin/zerepoch-pab-examples --config=zerepoch-pab.yaml webserver
  '';

  # For dev usage
  start-all-servers = pkgs.writeShellScriptBin "zerepoch-pab-all-servers" ''
    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    # There might be local modifications so only copy when missing
    ! test -f ./zerepoch-pab.yaml && cp ../zerepoch-pab/zerepoch-pab.yaml.sample zerepoch-pab.yaml
    # Only execute the migration when the database file does not exist
    ! test -f ./$(yq -r '.dbConfig.dbConfigFile' zerepoch-pab.yaml) && zerepoch-pab-migrate
    $(nix-build ../default.nix --quiet --no-build-output -A zerepoch-pab.server-examples-invoker)/bin/zerepoch-pab-examples --config=zerepoch-pab.yaml all-servers
  '';

  # For dev usage
  start-all-servers-m = pkgs.writeShellScriptBin "zerepoch-pab-all-servers-m" ''
    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    # There might be local modifications so only copy when missing
    ! test -f ./zerepoch-pab.yaml && cp ../zerepoch-pab/zerepoch-pab.yaml.sample zerepoch-pab.yaml
    # Only execute the migration when the database file does not exist
    ! test -f ./$(yq -r '.dbConfig.dbConfigFile' zerepoch-pab.yaml) && zerepoch-pab-migrate
    $(nix-build ../default.nix --quiet --no-build-output -A zerepoch-pab.server-examples-invoker)/bin/zerepoch-pab-examples --config=zerepoch-pab.yaml -m all-servers
  '';

  cleanSrc = gitignore-nix.gitignoreSource ./.;

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
  };

  client =
    buildPursPackage {
      inherit pkgs nodeModules;
      src = cleanSrc;
      name = "zerepoch-pab-client";
      extraSrcs = {
        web-common = webCommon;
        web-common-zerepoch = webCommonZerepoch;
        generated = generated-purescript;
      };
      # ideally we would just use `npm run test` but
      # this executes `spago` which *always* attempts to download
      # remote files (which obviously fails in sandboxed builds)
      checkPhase = ''
        node -e 'require("./output/Test.Main").main()'
      '';
      packages = pkgs.callPackage ./packages.nix { };
      spagoPackages = pkgs.callPackage ./spago-packages.nix { };
    };

  pab-exes = haskell.packages.zerepoch-pab.components.exes;

  demo-scripts = pkgs.callPackage ./pab-demo-scripts.nix { inherit client pab-exes; };

  mkConf = pkgs.callPackage ./config.nix { };

in
{
  inherit client demo-scripts server-examples-invoker server-setup-invoker test-generator generated-purescript generate-purescript migrate start-backend start-all-servers start-all-servers-m mkConf pab-exes;
}
