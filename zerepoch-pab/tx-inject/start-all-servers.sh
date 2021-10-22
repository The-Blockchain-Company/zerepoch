#!/bin/bash
rm -f ./primary.db
cabal build zerepoch-pab
cabal exec zerepoch-pab -- --config=./config.yaml migrate primary.db
cabal exec zerepoch-pab -- --config=./config.yaml contracts install --path "$(stack path --local-install-root)/bin/zerepoch-currency"
cabal exec zerepoch-pab -- --config=./config.yaml contracts install --path "$(stack path --local-install-root)/bin/zerepoch-atomic-swap"
cabal exec zerepoch-pab -- --config=./config.yaml contracts install --path "$(stack path --local-install-root)/bin/zerepoch-game"
cabal exec zerepoch-pab -- --config=./config.yaml contracts install --path "$(stack path --local-install-root)/bin/zerepoch-pay-to-wallet"
cabal exec zerepoch-pab -- --config=./config.yaml contracts install --path "$(stack path --local-install-root)/bin/prism-credential-manager"
cabal exec zerepoch-pab -- --config=./config.yaml contracts install --path "$(stack path --local-install-root)/bin/prism-mirror"
cabal exec zerepoch-pab -- --config=./config.yaml contracts install --path "$(stack path --local-install-root)/bin/prism-unlock-sto"
cabal exec zerepoch-pab -- --config=./config.yaml contracts install --path "$(stack path --local-install-root)/bin/prism-unlock-exchange"
cabal exec zerepoch-pab -- --config=./config.yaml all-servers "$1"
