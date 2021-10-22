{ config, lib, pkgs, ... }:
let
  inherit (lib) types mkOption mkIf;
  cfg = config.services.zerepoch-playground;
in
{
  options.services.zerepoch-playground = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        If enabled the zerepoch-playground server will be started.
      '';
    };

    webghcURL = mkOption {
      type = types.str;
      default = "http://localhost:4000";
      description = ''
        The webghc endpoint serving /runghc for compilation requests.
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

    port = mkOption {
      type = types.port;
      default = 4000;
      description = ''
        Port the zerepoch-playground server should bind to.
      '';
    };
    playground-server-package = mkOption {
      type = types.package;
      description = ''
        zerepoch playground package to execute.
      '';
    };
  };

  config = mkIf cfg.enable {

    systemd.services.zerepoch-playground = {
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

      script = ''
        if [ -f /var/lib/playgrounds/zerepoch.env ]; then
          echo "Loading environment config from '/var/lib/playgrounds/zerepoch.env'"
          source /var/lib/playgrounds/zerepoch.env
        else
          echo "No environment config. Using defaults"
        fi

        export WEBGHC_URL=${cfg.webghcURL}
        export FRONTEND_URL=${cfg.frontendURL}
        export GITHUB_CALLBACK_PATH=${cfg.githubCallbackPath}

        ${cfg.playground-server-package}/bin/zerepoch-playground-server webserver -p ${builtins.toString cfg.port};
      '';
    };
  };

}
