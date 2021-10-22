{ pkgs, gitignore-nix, webCommon, webCommonSimeon, buildPursPackage, buildNodeModules, filterNpm }:
let
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
    name = "simeon-marketplace-client";
    extraSrcs = {
      web-common = webCommon;
      web-common-simeon = webCommonSimeon;
    };
    packages = pkgs.callPackage ./packages.nix { };
    spagoPackages = pkgs.callPackage ./spago-packages.nix { };
  };
in
{
  inherit client;
}
