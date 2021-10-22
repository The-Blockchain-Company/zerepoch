A small tool to help with rapid interation when working with Zerepoch IR compiler.

For instance, when debugging an issue when compiling a file from the `simeon`
package, we can:

- dump the PIR of the troublesome Zerepoch IR term,
- modify the compiler, and
- re-run the compiler on the dumped PIR term, without the need to rebuild
  `simeon` and all of its dependencies.

# Dumping PIR

Zerepoch plugin supports dumping binary representation of the PIR via `dump-pir-flat`:

```haskell
{-# OPTIONS_GHC -fplugin-opt ZerepochTx.Plugin:dump-pir-flat #-}
```

Ideally, we would want to use the module when naming the PIR binary dump file,
but for the time being it received a random name.

# Debugging PIR Compilation

When dealing with issues with PIR compilation, we can simply re-run the
compiler on the dumped PIR flat file:

```bash
cabal run zerepoch-core:pir -- FILE.flat
```

`cabal run` should take care of rebuilding the compiler, so the issue of stale
plugin does not arise.

# Profiling PIR -> PLC Compilation

We can also profile the evaluation.

```bash
cabal run zerepoch-core:pir --enable-profiling -- FILE.flat
```

