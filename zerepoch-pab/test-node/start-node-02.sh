#!/bin/bash

cabal exec bcc-node -- run \
    --config configuration.yaml \
    --topology node2/topology.json \
    --database-path node2/db \
    --socket-path node2/node.sock \
    --sophie-kes-key node2/kes.skey \
    --sophie-vrf-key genesis/delegate-keys/delegate-key2.vrf.skey \
    --sophie-operational-certificate node2/cert \
    --port 3002
