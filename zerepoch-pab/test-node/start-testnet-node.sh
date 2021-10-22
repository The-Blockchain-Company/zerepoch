#!/bin/bash

cabal exec bcc-node -- run \
    --config testnet/testnet-config.json \
    --topology testnet/testnet-topology.json \
    --database-path testnet/db \
    --socket-path testnet/node.sock \
    --port 3003
