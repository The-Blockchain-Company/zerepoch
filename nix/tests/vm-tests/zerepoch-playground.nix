{ makeTest, writeText, zerepoch-playground }:
let
  envFile = writeText "zerepoch.env" ''
    JWT_SIGNATURE="yadayadayada"
    FRONTEND_URL="http://localhost:8080"
    GITHUB_CALLBACK_PATH="/#/gh-oauth-cb"
    GITHUB_CLIENT_ID="314123123a312fe"
    GITHUB_CLIENT_SECRET="kljfks234dskjhfeskjr"
  '';
in
makeTest {
  name = "zerepoch-playground";
  skipLint = true;
  machine = { pkgs, ... }:
    {
      imports = [ ../../modules/zerepoch-playground.nix ];
      environment.systemPackages = with pkgs; [ curl ];
      services.zerepoch-playground = {
        enable = true;
        port = 4000;
        webghcURL = "http://localhost:4000";
        frontendURL = "http://localhost:4000";
        githubCallbackPath = "/#/gh-oauth-cb";
        playground-server-package = zerepoch-playground.server;
      };
    };
  testScript = ''
    # fmt: off
    machine.start()
    machine.succeed("systemctl start zerepoch-playground")
    machine.wait_for_unit("zerepoch-playground.service")
    machine.wait_for_open_port(4000)
  '';

}
