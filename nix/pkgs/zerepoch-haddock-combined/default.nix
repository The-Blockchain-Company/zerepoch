{ haddock-combine, haskell, haskell-nix, writeTextFile }:
let
  toHaddock = haskell-nix.haskellLib.collectComponents' "library" haskell.projectPackagesAllHaddock;
in
haddock-combine {
  hspkgs = builtins.attrValues toHaddock;
  prologue = writeTextFile {
    name = "prologue";
    text = ''
      = Combined documentation for all the public Zerepoch libraries

      == Handy module entrypoints

        * "ZerepochTx": Compiling Haskell to PLC (Zerepoch Core; on-chain code).
        * "ZerepochTx.Prelude": Haskell prelude replacement compatible with PLC.
        * "Zerepoch.Contract": Writing Zerepoch apps (off-chain code).
        * "Ledger.Constraints": Constructing and validating Zerepoch
           transactions. Built on "ZerepochTx" and 
           "Zerepoch.Contract".
        * "Ledger.Typed.Scripts": A type-safe interface for spending and
           producing script outputs. Built on "ZerepochTx".
        * "Zerepoch.Trace.Emulator": Testing Zerepoch contracts in the emulator.
    '';
  };
}
