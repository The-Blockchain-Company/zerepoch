{ simeon-playground, zerepoch-playground, web-ghc, simeon-pab, simeon-dashboard, simeon-web, docs, pkgs }:
let
  staticSite = pkgs.callPackage ./static-site.nix { };
  playgroundStatic = pkgs.callPackage ./playground-static.nix { inherit staticSite; docs = docs.site; };
in
{
  web-ghc-server-entrypoint = pkgs.callPackage ./web-ghc-server.nix {
    web-ghc-server = web-ghc;
  };

  zerepoch-playground-server-entrypoint = pkgs.callPackage ./zerepoch-playground-server.nix {
    variant = "zerepoch";
    pkg = zerepoch-playground.server;
    port = 4003;
  };
  zerepoch-playground-client-entrypoint = playgroundStatic {
    client = zerepoch-playground.client;
    variant = "zerepoch";
    port = 8081;
  };

  simeon-playground-server-entrypoint = pkgs.callPackage ./zerepoch-playground-server.nix {
    variant = "simeon";
    pkg = simeon-playground.server;
    port = 4004;
  };
  simeon-playground-client-entrypoint = playgroundStatic {
    client = simeon-playground.client;
    variant = "simeon";
    port = 8087;
  };

  simeon-run-entrypoint = pkgs.callPackage ./pab.nix {
    pabExe = "${simeon-pab}/bin/simeon-pab";
    staticPkg = simeon-dashboard.client;
  };

  simeon-website-entrypoint = staticSite {
    root = simeon-web;
    port = 8088;
  };
}
