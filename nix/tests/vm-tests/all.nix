{ makeTest
, lib
, docs
, zerepoch-pab
, simeon-pab
, zerepoch-playground
, simeon-playground
, simeon-dashboard
, web-ghc
, vmCompileTests # when enabled the test tries to compile zerepoch/simeon code on webghc
}:
let
  zerepochApiRequest = builtins.toFile "zerepoch-request.json" (builtins.readFile ./contract-api-request.json);
  simeonApiRequest = builtins.toFile "simeon-request.json" (builtins.readFile ./runghc-api-request.json);
in
makeTest {
  skipLint = true;
  name = "all";
  nodes = {

    # ---------------------------------------------------------------------------------------------------------------
    # pab : 192.168.1.1 - running zerepoch pab
    # --------------------------------------------------------------------------------------------------------------

    pab = { pkgs, ... }: {
      imports = [ ../../modules/pab.nix ];
      environment.systemPackages = with pkgs; [ curl ];

      networking = {
        firewall.allowedTCPPorts = [ 8080 8081 8082 8083 8084 ];
        dhcpcd.enable = false;
        interfaces.eth1.ipv6.addresses = lib.mkOverride 0 [{ address = "fd00::1"; prefixLength = 64; }];
        interfaces.eth1.ipv4.addresses = lib.mkOverride 0 [{ address = "192.168.1.1"; prefixLength = 24; }];
      };

      services.pab = {
        enable = true;
        pab-setup = zerepoch-pab.pab-exes.zerepoch-pab-setup;
        pab-executable = "${simeon-pab}/bin/simeon-pab";
        staticContent = simeon-dashboard.client;
        dbFile = "/var/lib/pab/pab-core.db";
        defaultWallet = 1;
        webserverPort = 8080;
        walletPort = 8081;
        nodePort = 8082;
        chainIndexPort = 8083;
        signingProcessPort = 8084;
      };
    };

    # ---------------------------------------------------------------------------------------------------------------
    # playgrounds : 192.168.1.2 - running zerepoch/simeon playgrounds and nginx
    # --------------------------------------------------------------------------------------------------------------

    playgrounds = { pkgs, ... }: {
      imports = [
        ../../modules/zerepoch-playground.nix
        ../../modules/simeon-playground.nix
      ];

      networking = {
        firewall.allowedTCPPorts = [ 7070 8080 9090 ];
        extraHosts = ''
          127.0.0.1 zerepoch-playground
          127.0.0.1 simeon-playground
          127.0.0.1 simeon-dashboard
          192.168.1.1 pab
          192.168.1.3 webghc
        '';
        dhcpcd.enable = false;
        interfaces.eth1.ipv6.addresses = lib.mkOverride 0 [{ address = "fd00::2"; prefixLength = 64; }];
        interfaces.eth1.ipv4.addresses = lib.mkOverride 0 [{ address = "192.168.1.2"; prefixLength = 24; }];
      };

      services = {
        simeon-playground = {
          enable = true;
          port = 4001;
          playground-server-package = simeon-playground.server;
        };

        zerepoch-playground = {
          enable = true;
          port = 4000;
          playground-server-package = zerepoch-playground.server;
          webghcURL = "http://webghc";
        };

        nginx = {
          enable = true;
          recommendedGzipSettings = true;
          recommendedProxySettings = true;
          recommendedOptimisation = true;

          upstreams = {
            zerepoch-playground.servers."127.0.0.1:4000" = { };
            simeon-playground.servers."127.0.0.1:4001" = { };
            simeon-dashboard.servers."192.168.1.1:8080" = { };
          };
          virtualHosts = {
            "simeon-dashboard" = {
              listen = [{ addr = "0.0.0.0"; port = 7070; }];
              locations = {
                "/" = {
                  proxyPass = "http://192.168.1.1:8080";
                };
              };
            };
            "zerepoch-playground" = {
              listen = [{ addr = "0.0.0.0"; port = 8080; }];
              locations = {
                "/api" = {
                  proxyPass = "http://zerepoch-playground";
                  proxyWebsockets = true;
                };
                "^~ /doc/" = {
                  alias = "${docs.site}/";
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
                "/" = {
                  root = "${zerepoch-playground.client}";
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
                "@fallback" = {
                  proxyPass = "http://zerepoch-playground";
                  proxyWebsockets = true;
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
              };
            };
            "simeon-playground" = {
              listen = [{ addr = "0.0.0.0"; port = 9090; }];
              locations = {
                "/" = {
                  root = "${simeon-playground.client}";
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
                "^~ /doc/" = {
                  alias = "${docs.site}/";
                  extraConfig = ''
                    error_page 404 = @fallback;
                  '';
                };
                "/runghc" = {
                  proxyPass = "http://webghc";
                };
                "@fallback" = {
                  proxyPass = "http://simeon-playground";
                  proxyWebsockets = true;
                };
              };
            };
          };
        };
      };

      environment.systemPackages = with pkgs; [ curl ];
    };

    # ---------------------------------------------------------------------------------------------------------------
    # webghc : 192.168.1.3 - running webghc with zerepoch/simeon deps
    # --------------------------------------------------------------------------------------------------------------

    webghc = { pkgs, ... }: {

      virtualisation.memorySize = "1024";

      networking = {
        firewall.allowedTCPPorts = [ 80 ];
        dhcpcd.enable = false;
        interfaces.eth1.ipv6.addresses = lib.mkOverride 0 [{ address = "fd00::3"; prefixLength = 64; }];
        interfaces.eth1.ipv4.addresses = lib.mkOverride 0 [{ address = "192.168.1.3"; prefixLength = 24; }];
      };

      imports = [
        ../../modules/web-ghc.nix
      ];
      services = {
        web-ghc = {
          enable = true;
          port = 80;
          web-ghc-package = web-ghc;
        };
      };
    };
  };
  testScript = ''
    playgrounds.start()
    webghc.start()
    pab.start()

    #
    # assert connectivity
    #
    playgrounds.wait_for_unit("network-online.target")
    pab.wait_for_unit("network-online.target")
    pab.wait_for_unit("pab.service")

    # Refer to `services.pab` configuration  above to see what
    # service each individual port relates to.
    pab.wait_for_open_port(8080)
    pab.wait_for_open_port(8081)
    pab.wait_for_open_port(8082)
    pab.wait_for_open_port(8083)
    webghc.wait_for_unit("network-online.target")
    playgrounds.succeed("ping -c1 192.168.1.1")
    playgrounds.succeed("ping -c1 192.168.1.2")
    playgrounds.succeed("ping -c1 192.168.1.3")
    pab.succeed("ping -c1 192.168.1.1")
    pab.succeed("ping -c1 192.168.1.2")
    pab.succeed("ping -c1 192.168.1.3")
    webghc.succeed("ping -c1 192.168.1.1")
    webghc.succeed("ping -c1 192.168.1.2")
    webghc.succeed("ping -c1 192.168.1.3")


    #
    # playground / frontend asserts
    #
    playgrounds.wait_for_unit("simeon-playground.service")
    playgrounds.wait_for_unit("zerepoch-playground.service")
    playgrounds.wait_for_unit("nginx.service")
    playgrounds.wait_for_open_port(7070)
    playgrounds.wait_for_open_port(8080)
    playgrounds.wait_for_open_port(9090)

    with subtest("********************************************************************************************* TEST: All content is being served on playgrounds"):
      res = playgrounds.succeed("curl --silent http://zerepoch-playground:8080/")
      assert "zerepoch" in res, "Expected string 'zerepoch' from 'http://zerepoch-playground:8080'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://zerepoch-playground:8080/doc/")
      assert "The Zerepoch Platform" in res, "Expected string 'The Zerepoch Platform' from 'http://zerepoch-playground:8080/doc/'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://zerepoch-playground:8080/doc/zerepoch/tutorials/")
      assert "The Zerepoch Platform" in res, "Expected string 'Tutorials' from 'http://zerepoch-playground:8080/doc/zerepoch/tutorials/'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://simeon-playground:9090/")
      assert "simeon-playground" in res, "Expected string 'simeon-playground' from 'http://simeon-playground:9090'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://simeon-playground:9090/doc/")
      assert "simeon-playground" in res, "Expected string 'The Zerepoch Platform' from 'http://simeon-playground:9090/doc'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent http://simeon-playground:9090/doc/simeon/tutorials/")
      assert "Tutorials" in res, "Expected string 'Tutorials' from 'http://simeon-playground:9090/doc/simeon/tutorials'. Actual: {}".format(res)

      #res = playgrounds.succeed("curl --silent http://simeon-dashboard:7070/")
      #assert "simeon-dashboard" in res, "Expected string 'simeon-dashboard' from 'http://simeon-dashboard:7070/'. Actual: {}".format(res)

      res = pab.succeed("curl --silent http://localhost:8080/")
      assert "simeon-dashboard" in res, "Expected string 'simeon-dashboard' from 'http://simeon-dashboard:7070/'. Actual: {}".format(res)

    #
    # webghc asserts
    #
    webghc.wait_for_unit("web-ghc.service")
    webghc.wait_for_open_port(80)

    #
    # pab asserts
    #

  '' + lib.optionalString (vmCompileTests) ''
    #
    # zerepoch-playground / webghc : using api/contract
    # simeon-playground / webghc : using /runghc
    #
    with subtest("********************************************************************************************* TEST: compilation works"):
      res = playgrounds.succeed("curl --silent -H 'Content-Type: application/json' --request POST --data @${zerepochApiRequest} http://zerepoch-playground:8080/api/contract")
      assert "Right" in res, "Expected response wrapped in 'Right'. Actual: {}".format(res)

      res = playgrounds.succeed("curl --silent -H 'Content-Type: application/json' --request POST --data @${simeonApiRequest} http://simeon-playground:9090/runghc")
      assert "Right" in res, "Expected response wrapped in 'Right'. Actual: {}".format(res)
  '';
}
