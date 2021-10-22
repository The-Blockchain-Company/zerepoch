#!/bin/bash

cabal exec bcc-node -- run \
    --config configuration.yaml \
    --topology node1/topology.json \
    --database-path node1/db \
    --socket-path node1/node.sock \
    --sophie-kes-key node1/kes.skey \
    --sophie-vrf-key genesis/delegate-keys/delegate1.vrf.skey \
    --sophie-operational-certificate node1/cert \
    --port 3001
