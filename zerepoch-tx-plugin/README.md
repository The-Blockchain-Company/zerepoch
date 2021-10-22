# zerepoch-tx-plugin: Zerepoch Tx compiler plugin

This contains the Zerepoch Tx compiler plugin itself. This should
be added as a dependency, but used via the functions in the
`zerepoch-tx` package.

This is in a separate package because it depends on `ghc`. Packages
which need to be cross-compiled should depend on this conditionally.
Use the following snippet:

.package.cabal
----
if !(impl(ghcjs) || os(ghcjs))
    build-depends: zerepoch-tx-plugin
----
