# Building

## Server

### stack

```sh
stack build zerepoch-playground-server
stack exec -- zerepoch-playground-server psgenerator ./zerepoch-playground-client/generated
stack exec -- zerepoch-playground-server webserver
```

### nix

```sh
$(nix-build -A zerepoch-playground.server)/bin/zerepoch-playground-server webserver
```

## Testing

Tests should be run with nix:

```sh
nix build -L -f default.nix zerepoch.haskell.packages.zerepoch-playground-server.checks
```
