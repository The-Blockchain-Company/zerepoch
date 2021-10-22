# Zerepoch Application Backend

_PAB_ (Zerepoch Application Backend) is an Off-Chain application for managing the state of
Zerepoch contract instances.

## Table of Contents

- [Building](#building)
- [Running](#running)
- [PAB Components](#pab-components)

## Building

_PAB_ is a Cabal based Haskell project. Production builds are done with Nix using [haskell.nix](https://github.com/The-Blockchain-Company/haskell.nix)

### Cabal

```
$ cabal build
```

### Nix

```
$ nix-build ../default.nix -A zerepoch.haskell.packages.zerepoch-pab
```

## Running

In order to use PAB several of its components need to be started. Furthermore the pab-client has
to be started in order to access the web frontend. The required steps are described below, assuming
nix has been installed.

First we build the startup scripts:

```
$ nix-build ../default.nix -A zerepoch-pab.demo-scripts
```

Next we start all required servers and install several contracts, in one terminal:

```
$ ./result/bin/pab-start-all-servers
```

Now we start an additional PAB connecting to the same node, in another terminal:

```
$ ./result/bin/pab-start-second-pab
```

Finally we can start the pab web frontend using tool provided by nix environment, in a third terminal:

```
$ nix-shell
$ cd ../zerepoch-pab-client
$ npm start
```

The client is now running at https://localhost:8009 -- See [pab-demo-scripts.nix](https://github.com/The-Blockchain-Company/zerepoch/blob/pab-readme/zerepoch-pab-client/pab-demo-scripts.nix) for details on the service invcation and contract installation.

**Note**: By default the frontend will forward requests to `localhost:9080` - the first PAB instance. Connecting to the second
instance currently requires changing the proxy config in [webpack.config.js](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab-client/webpack.config.js#L33-L41). The second instance runs on port 9086 so the linked section in the config file would have to be
updated accordingly.

## PAB Components
PAB contains several commands and services, which are outlined below.

- [psgenerator](#psgenerator)
- [migrate](#migrate)
- [all-servers](#all-servers)
- [client-services](#client-services)
- [wallet-server](#wallet-server)
- [webserver](#webserver)
- [node-server](#node-server)
- [chain-index](#chain-index)
- [metadata-server](#metadata-server)
- [signing-process](#signing-process)
- [default-logging-config](#default-logging-config)
- [process-outboxes](#process-outboxes)

### psgenerator

```
$ cabal run zerepoch-pab -- psgenerator
```

#### Description

Generates the purescript bridge code.

#### Source

- [app/PSGenerator.hs](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/app/PSGenerator.hs)

### migrate

```
$ cabal run zerepoch-pab -- migrate
```

#### Description

Migrates the database in `pab-core.db` to the current schema.  The database contains the state for the contract instances.

#### Source
[Zerepoch.PAB.App.migrate](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Zerepoch/PAB/App.hs#L283)


#### all-servers

```
$ cabal run zerepoch-pab -- all-servers
```

#### Description

Combines the execution of all core services and mocks in the appropriate order:

* mocks
    - mock node
    - mock wallet
    - metadata
    - signing-progress
* core services
    - PAB webserver
    - chain index
    - process-outboxes

#### Dependencies

- zerepoch-pab.yaml
- sqlite database
- pab-client


#### Source

- [app/Main.hs](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/app/Main.hs#L246)

### client-services

```
$ cabal run zerepoch-pab -- client-services
```

#### Description

Starts all mocks and core services *except for* the mock node service:

* mocks
    - mock wallet
    - metadata
    - signing-progress
* core services
    - PAB webserver
    - chain index
    - process-outboxes

#### Dependencies

- zerepoch-pab.yaml
- sqlite database
- pab-client


#### Source

- [app/Main.hs](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/app/Main.hs#L262)

### wallet-server

```
$ cabal run zerepoch-pab -- wallet-server
```

#### Description

Zerepoch specific wallet implementation for managing user funds on the blockchain. Clients to this service are:

- PAB: adding funds to transactions & signing transactions
- user: making payments

#### Dependencies

- zerepoch-pab.yaml
- mock node

#### Source

- [Bcc.Wallet.API](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Bcc/Wallet/API.hs)
- [Bcc.Wallet.Server.main](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Bcc/Wallet/Server.hs#L101)
- [Bcc.Wallet.Types.Config](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Bcc/Wallet/Types.hs#L47)

### webserver

```
$ cabal run zerepoch-pab -- webserver
```

#### Description

Serves the PAB user interface

#### Dependencies

- zerepoch-pab.yaml
- sqlite database
- pab-client

#### Source

- [Zerepoch.PAB.Webserver.API](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Zerepoch/PAB/Webserver/API.hs#L23)
- [Zerepoch.PAB.Webserver.Server.main](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Zerepoch/PAB/Webserver/Server.hs)

### node-server

```
$ cabal run zerepoch-pab -- node-server
```

#### Description

Mock-implementation of a Charles node. Clients to this service are:

- chain-index
- webserver
- mock wallet

#### Dependencies

- zerepoch-pab.yaml

#### Source

- [Bcc.Node.API.API](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Bcc/Node/API.hs)
- [Bcc.Node.Server.main](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Bcc/Node/Server.hs)

### chain-index

```
$ cabal run zerepoch-pab -- chain-index
```

#### Description

Provides a consistent view of the current state of the blockchain including a key-value store
for datum and script hashes. Clients to this service are:

- process-outboxes

#### Dependencies

- zerepoch-pab.yaml
- mock node

#### Source

- [Bcc.ChainIndex.API.API](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Bcc/ChainIndex/API.hs)
- [Bcc.ChainIndex.Server.main](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Bcc/ChainIndex/Server.hs#L69)

### metadata-server

```
$ cabal run zerepoch-pab -- metadata-server
```

#### Description

Key-Value store for user-defined data (mock implementation)

#### Dependencies

- zerepoch-pab.yaml

#### Source

- [Bcc.Metadata.API.API](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Bcc/Metadata/API.hs)
- [Bcc.Metadata.Server.main](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Bcc/Metadata/Server.hs#L196)

### signing-process

```
$ cabal run zerepoch-pab -- signing-process
```

#### Description

Attaches signatures to transactions so that they can be sent to the node. Clients to this service are:

- PAB
- mock wallet

#### Dependencies

- zerepoch-pab.yaml

#### Source

- [Bcc.SigningProcess.API.API](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Bcc/SigningProcess/API.hs#L11)
- [Bcc.SigningProcess.Server.main](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Bcc/SigningProcess/Server.hs#L64)

### default-logging-config

```
$ cabal run zerepoch-pab -- default-logging-config
```

#### Description

Prints the default logging configuration to STDOUT

#### Source

- [app/Main.hs](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/app/Main.hs#L479)

### contracts process-outboxes

```
$ cabal run zerepoch-pab -- contracts process-outboxes
```

#### Description

A service that regularly looks at the contract instances requests and handles them

#### Dependencies

- zerepoch-pab.yaml
- sqlite database
- pab-client

#### Source

- [Zerepoch.PAB.Core.processAllContractOutboxes](https://github.com/The-Blockchain-Company/zerepoch/blob/master/zerepoch-pab/src/Zerepoch/PAB/Core/ContractInstance.hs#L585)
