{ pkgs, config, lib, tfinfo, ... }:
{

  imports = [
    ./std.nix
    ../../../nix/modules/zerepoch-playground.nix
    ../../../nix/modules/simeon-playground.nix
  ];

  networking = {
    firewall.allowedTCPPorts = [ 22 80 8080 8181 9080 ];
  };

  services.simeon-playground = {
    enable = true;
    webghcURL = "http://${tfinfo.environment}.${tfinfo.zerepochTld}";
    port = 4001;
    frontendURL =
      if tfinfo.environment == "production"
      then "https://play.simeon-finance.io"
      else "https://${tfinfo.environment}.${tfinfo.simeonTld}";
    playground-server-package = pkgs.simeon-playground.server;
  };

  services.zerepoch-playground = {
    enable = true;
    port = 4000;
    webghcURL = "http://${tfinfo.environment}.${tfinfo.zerepochTld}";
    frontendURL = "https://${tfinfo.environment}.${tfinfo.zerepochTld}";
    playground-server-package = pkgs.zerepoch-playground.server;
  };

  services.nginx =
    let
      staticFileCacheControl = ''
        # static files should not be too costly to serve so we can allow more generous rates
        limit_req zone=staticlimit burst=1000;
        add_header 'Cache-Control' 'no-store, no-cache, must-revalidate, proxy-revalidate, max-age=0';
        expires off;
      '';
      versionConfig = ''
        default_type application/json;
        return 200 '{"rev": "${tfinfo.rev}"}';
      '';
    in
    {
      enable = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      recommendedOptimisation = true;

      appendHttpConfig = ''
        limit_req_zone $binary_remote_addr zone=zerepochlimit:10m rate=2r/s;
        limit_req_zone $binary_remote_addr zone=staticlimit:500m rate=100r/s;
        server_names_hash_bucket_size 128;
        log_format compression '$remote_addr - $remote_user [$time_local] '
        '"$request" $status $body_bytes_sent '
        '"$http_referer" "$http_user_agent" "$gzip_ratio"';
      '';

      upstreams = {
        zerepoch-playground.servers."127.0.0.1:4000" = { };
        simeon-playground.servers."127.0.0.1:4001" = { };
      };
      virtualHosts = {
        "simeon-web" = {
          listen = [{ addr = "0.0.0.0"; port = 8181; }];
          locations = {
            "/" = {
              root = "${pkgs.simeon-web}";
              extraConfig = ''
                ${staticFileCacheControl}
              '';
            };
          };
        };
        "zerepoch-playground" = {
          listen = [{ addr = "0.0.0.0"; port = 8080; }];
          locations = {
            "/version" = {
              extraConfig = versionConfig;
            };
            "/health" = {
              proxyPass = "http://zerepoch-playground";
            };
            "/" = {
              root = "${pkgs.zerepoch-playground.client}";
              extraConfig = ''
                ${staticFileCacheControl}
                error_page 404 = @fallback;
              '';
            };
            "^~ /doc/" = {
              alias = "${pkgs.zerepoch-docs.site}/";
              extraConfig = ''
                error_page 404 = @fallback;
              '';
            };
            "@fallback" = {
              proxyPass = "http://zerepoch-playground";
              proxyWebsockets = true;
              extraConfig = ''
                limit_req zone=zerepochlimit burst=10;
              '';
            };
          };
        };
        "simeon-playground" = {
          listen = [{ addr = "0.0.0.0"; port = 9080; }];
          locations = {
            "/version" = {
              extraConfig = versionConfig;
            };
            "/health" = {
              proxyPass = "http://simeon-playground";
            };
            "/" = {
              root = "${pkgs.simeon-playground.client}";
              extraConfig = ''
                ${staticFileCacheControl}
                error_page 404 = @fallback;
              '';
            };
            "^~ /doc/" = {
              alias = "${pkgs.zerepoch-docs.site}/";
              extraConfig = ''
                error_page 404 = @fallback;
              '';
            };
            "@fallback" = {
              proxyPass = "http://simeon-playground";
              proxyWebsockets = true;
              extraConfig = ''
                limit_req zone=zerepochlimit burst=10;
              '';
            };
          };
        };
      };
    };

  deployment = {
    secrets = {
      "zerepoch-secrets" = {
        source = "./secrets.zerepoch.${tfinfo.environment}.env";
        destination = "/var/lib/playgrounds/zerepoch.env";
        action = [ "systemctl" "restart" "zerepoch-playground" ];
        permissions = "0444";
      };
      "simeon-secrets" = {
        source = "./secrets.simeon.${tfinfo.environment}.env";
        destination = "/var/lib/playgrounds/simeon.env";
        action = [ "systemctl" "restart" "simeon-playground" ];
        permissions = "0444";
      };

    };
    healthChecks = {
      cmd = [
        {
          cmd = [ "systemctl" "status" "zerepoch-playground.service" ];
          description = "Check if zerepoch-playground systemd service is running";
        }
        {
          cmd = [ "systemctl" "status" "simeon-playground.service" ];
          description = "Check if simeon-playground systemd service is running";
        }
      ];
    };
  };

}
