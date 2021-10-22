{ makeTest, writeText, simeon-playground }:
let
  envFile = writeText "simeon.env" ''
    JWT_SIGNATURE="yadayadayada"
    FRONTEND_URL="http://localhost:8080"
    GITHUB_CALLBACK_PATH="/#/gh-oauth-cb"
    GITHUB_CLIENT_ID="314123123a312fe"
    GITHUB_CLIENT_SECRET="kljfks234dskjhfeskjr"
  '';
in
makeTest {
  name = "simeon";
  skipLint = true;
  machine = { pkgs, ... }:
    {
      imports = [ ../../modules/simeon-playground.nix ];
      environment.systemPackages = with pkgs; [ curl ];
      services.simeon-playground = {
        enable = true;
        port = 4001;
        webghcURL = "http://localhost:4000";
        frontendURL = "http://localhost:4000";
        githubCallbackPath = "/#/gh-oauth-cb";
        playground-server-package = simeon-playground.server;
      };
    };
  testScript = ''
    # fmt: off
    machine.start()
    machine.succeed("systemctl start simeon-playground")
    machine.wait_for_unit("simeon-playground.service")
    machine.wait_for_open_port(4001)
    machine.succeed("curl localhost:4001/health")
  '';

}
