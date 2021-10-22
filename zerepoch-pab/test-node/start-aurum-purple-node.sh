#!/bin/bash

cabal exec bcc-node -- run \
    --config aurum-purple/aurum-purple-config.json \
    --topology aurum-purple/aurum-purple-topology.json \
    --database-path aurum-purple/db \
    --socket-path aurum-purple/node-server.sock \
    --port 3003
