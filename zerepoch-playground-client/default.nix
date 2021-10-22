{ pkgs, lib, gitignore-nix, haskell, webCommon, webCommonZerepoch, webCommonPlayground, buildPursPackage, buildNodeModules, filterNpm }:
let
  playground-exe = haskell.packages.zerepoch-playground-server.components.exes.zerepoch-playground-server;

  build-playground-exe = "$(nix-build --quiet --no-build-output ../default.nix -A zerepoch.haskell.packages.zerepoch-playground-server.components.exes.zerepoch-playground-server)";

  build-ghc-with-zerepoch = "$(nix-build --quiet --no-build-output -E '(import ./.. {}).zerepoch.haskell.project.ghcWithPackages(ps: [ ps.zerepoch-core ps.zerepoch-tx ps.zerepoch-contract ps.zerepoch-ledger ps.playground-common ])')";

  # Output containing the purescript bridge code
  # We need to add ghc with dependecies because `psgenerator` needs to invoke ghc to
  # create test data.
  generated-purescript =
    let
      ghcWithZerepoch = haskell.project.ghcWithPackages (ps: [ ps.zerepoch-core ps.zerepoch-tx ps.zerepoch-contract ps.zerepoch-ledger ps.playground-common ]);
    in
    # For some reason on darwin GHC will complain bout missing otool, I really don't know why
    pkgs.runCommand "zerepoch-playground-purescript" { buildInputs = lib.optional pkgs.stdenv.isDarwin [ pkgs.darwin.cctools ]; } ''
      PATH=${ghcWithZerepoch}/bin:$PATH
      mkdir $out
      ${playground-exe}/bin/zerepoch-playground-server psgenerator $out
    '';

  # generate-purescript: script to create purescript bridge code
  #
  # * Note-1: We need to add ghc to the path because the purescript generator
  # actually invokes ghc to generate test data so we need ghc with the necessary deps
  #
  # * Note-2: This command is supposed to be available in the nix-shell but want
  # to avoid zerepoch-core in the shell closure so we do $(nix-build ..) instead
  generate-purescript = pkgs.writeShellScriptBin "zerepoch-playground-generate-purs" ''
    GHC_WITH_PKGS=${build-ghc-with-zerepoch}
    export PATH=$GHC_WITH_PKGS/bin:$PATH

    rm -rf ./generated
    ${build-playground-exe}/bin/zerepoch-playground-server psgenerator generated
  '';

  # start-backend: script to start the zerepoch-playground-server
  #
  # Note-1: We need to add ghc to the path because the server provides /runghc
  # which needs ghc and dependencies.
  # Note-2: We want to avoid to pull the huge closure in so we use $(nix-build) instead
  start-backend = pkgs.writeShellScriptBin "zerepoch-playground-server" ''
    echo "zerepoch-playground-server: for development use only"
    GHC_WITH_PKGS=${build-ghc-with-zerepoch}
    export PATH=$GHC_WITH_PKGS/bin:$PATH

    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    export GITHUB_CALLBACK_PATH=https://localhost:8009/api/oauth/github/callback

    ${build-playground-exe}/bin/zerepoch-playground-server webserver "$@"
  '';

  cleanSrc = gitignore-nix.gitignoreSource ./.;

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
  };

  client = buildPursPackage {
    inherit pkgs nodeModules;
    src = cleanSrc;
    name = "zerepoch-playground-client";
    # ideally we would just use `npm run test` but
    # this executes `spago` which *always* attempts to download
    # remote files (which obviously fails in sandboxed builds)
    checkPhase = ''
      node -e 'require("./output/Test.Main").main()'
    '';
    extraSrcs = {
      web-common = webCommon;
      web-common-zerepoch = webCommonZerepoch;
      web-common-playground = webCommonPlayground;
      generated = generated-purescript;
    };
    packages = pkgs.callPackage ./packages.nix { };
    spagoPackages = pkgs.callPackage ./spago-packages.nix { };
  };
in
{
  inherit client generate-purescript start-backend;
  server = playground-exe;
}
