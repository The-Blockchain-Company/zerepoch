{ config, lib, pkgs, ... }:
let
  inherit (lib) types mkOption mkIf;
  cfg = config.services.simeon-playground;

  killallz3 = pkgs.writeScriptBin "killallz3" ''
    kill -9 $(ps aux | grep z3 | grep -v grep | awk '{print $2}')
  '';

in
{
  options.services.simeon-playground = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        If enabled the simeon-playground server will be started.
      '';
    };

    webghcURL = mkOption {
      type = types.str;
      default = "http://localhost:4000";
      description = ''
        The webghc endpoint serving /runghc for compilation requests.
      '';
    };

    port = mkOption {
      type = types.port;
      default = 4001;
      description = ''
        Port the simeon-playground server should bind to.
      '';
    };

    frontendURL = mkOption {
      type = types.str;
      default = "http://localhost:4000";
      description = ''
        URL where the zerepoch playground is served.
      '';
    };

    githubCallbackPath = mkOption {
      type = types.str;
      default = "/#/gh-oauth-cb";
      description = ''
        The github callback path
      '';
    };

    playground-server-package = mkOption {
      type = types.package;
      description = ''
        simeon playground package to execute.
      '';
    };
  };

  config = mkIf cfg.enable {

    systemd.services.simeon-playground = {
      after = [ "network.target" ];
      wantedBy = [ "nginx.service" ];
      before = [ "nginx.service" ];

      serviceConfig = {
        # runtime behavior
        TimeoutStartSec = 5;
        TimeoutStopSec = 5;
        Restart = "always";

        # sane defaults for security
        DynamicUser = true;
        ProtectKernelTunables = true;
        ProtectControlGroups = true;
        ProtectKernelModules = true;
        PrivateDevices = true;
        SystemCallArchitectures = "native";

      };

      path = [ pkgs.z3 killallz3 ];
      script = ''
        if [ -f /var/lib/playgrounds/simeon.env ]; then
          echo "Loading environment config from '/var/lib/playgrounds/simeon.env'"
          source /var/lib/playgrounds/simeon.env
        else
          echo "No environment config. Using defaults"
        fi

        export WEBGHC_URL=${cfg.webghcURL}
        export FRONTEND_URL=${cfg.frontendURL}
        export GITHUB_CALLBACK_PATH=${cfg.githubCallbackPath}

        ${cfg.playground-server-package}/bin/simeon-playground-server webserver -p ${builtins.toString cfg.port}
      '';

    };
  };

}
