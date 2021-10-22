{ pkgs, gitignore-nix, haskell, webCommon, webCommonSimeon, buildPursPackage, buildNodeModules, filterNpm, simeon-dashboard }:
let
  cleanSrc = gitignore-nix.gitignoreSource ../simeon-dashboard-client/.;

  fake-pab-exe = haskell.packages.fake-pab.components.exes.fake-pab-server;

  # Output containing the purescript bridge code
  fake-pab-generated-purescript = pkgs.runCommand "simeon-fake-pab-purescript" { } ''
    mkdir $out
    ${fake-pab-exe}/bin/fake-pab-server psgenerator $out
  '';

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ../simeon-dashboard-client/package.json;
    packageLockJson = ../simeon-dashboard-client/package-lock.json;
    githubSourceHashMap = { };
  };

  client = buildPursPackage {
    inherit pkgs nodeModules;
    src = cleanSrc;
    checkPhase = ''
      node -e 'require("./output/Test.Main").main()'
    '';
    name = "simeon-dashboard-client-fake-pab";
    extraSrcs = {
      web-common = webCommon;
      web-common-simeon = webCommonSimeon;
      generated = simeon-dashboard.generated-purescript;
      fake-pab-generated = fake-pab-generated-purescript;
    };
    packages = pkgs.callPackage ../simeon-dashboard-client/packages.nix { };
    spagoPackages = pkgs.callPackage ../simeon-dashboard-client/spago-packages.nix { };
  };
in
{
  inherit client fake-pab-exe fake-pab-generated-purescript;
}
