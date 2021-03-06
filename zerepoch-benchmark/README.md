## Zerepoch Benchmarks

This directory contains two sets of benchmarks:

* `nofib`: Zerepoch versions of some of Haskell's `nofib` benchmarks from https://github.com/ghc/nofib.

   * The source for the individual programs is in `nofib/src`
   * There is an executable in `nofib/exe` which can be used to run the individual programs (compiled into Zerepoch)
   * The benchmarking code is in `nofib/bench`.

   * To run the benchmarks using stack, type a command like this
       * `stack bench zerepoch-benchmark:nofib` (run all benchmarks; this will take a long time)
       * `stack bench zerepoch-benchmark:nofib --ba "clausify/formula2 -L300"` (run the `clausify/formula2`
          benchmark with a time limit of 300 seconds)

   * The corresponding cabal commands are
       * `cabal bench zerepoch-benchmark:nofib`
       * `cabal bench zerepoch-benchmark:nofib --benchmark-options "clausify/formula2 -L300"`

   * With cabal, you can also say
       * `cabal run zerepoch-benchmark:nofib`
       * `cabal run zerepoch-benchmark:nofib -- clausify/formula2 -L300`

     The difference is that `cabal run` runs the benchmarks with the working directory
     set to the shell's current working directory, but `cabal bench` sets the working directory
     to the `zerepoch-benchmark` directory.  Stack uses the benchmark directory for its
     working directory; there doesn't seem to be any way to get it to use any other directory.

   * By default, the `nofib` benchmarks are run for a minimum of **60 seconds
     each** in order to get a statistically reasonable number of executions.
     You can change this with Criterion's `-L` option.  With the 60 second limit
     the entire suite takes perhaps 20-40 minutes to run (although this will
     depend on the hardware).

* `validation`:  a number of Zerepoch Core scripts extracted from the `zerepoch-use-cases` tests which represent realistic on-chain
   transaction validations.

   * The scripts are stored in flat-encoded form in various subdirectories of `validation/data`.

   * The benchmarking code is in `validation/Main.hs`.

   * To run the benchmarks using stack, type a command like this
       * `stack bench zerepoch-benchmark:validation` (run all benchmarks)
       * `stack bench zerepoch-benchmark:validation --ba "crowdfunding/2 -L60"` (run the `crowdfunding/2`
           benchmark with a time limit of 60 seconds)

   * The corresponding cabal commands are
       * `cabal bench zerepoch-benchmark:validation`
       * `cabal bench zerepoch-benchmark:validation --benchmark-options "crowdfunding/2 -L60"`
     or the `cabal run` equivalents (see the `nofib` section).

   * During benchmarking each validation script is run repeatedly up to a limit
     of 20 seconds (a single execution takes approximately 5-15 ms) to get
     statistically reasonable total number of executions.  It takes about 10
     minutes to run the entire suite (again, this will depend on the hardware).

See also [nofib/README.md](./nofib/README.md) and [validation/README.md](./validation/README.md).

### nofib-exe
The `nofib-exe` program in `nofib/exe` allows you to run the `nofib` benchmarks from the command line and
output Zerepoch Core versions in a number of formats.  See the built-in help information
for details.

### Criterion output

Both sets of benchmarks will generate a file called `report.html` containing
detailed information about the results of running the benchmarks. This will be
written to the `zerepoch-benchmark` directory.  To put it elsewhere, pass
Criterion the `--output` option along with an *absolute* path (relative paths
are interpreted relative to `zerepoch-benchmark` when running the benchmarks via
stack or cabal): for example

```
  stack bench zerepoch-benchmark:validation --ba "crowdfunding -L60 --output $PWD/crowdfunding-report.html"
```

If using `cabal bench` you'll have to do something similar, but `cabal run` will write the output into
the current directory (unless you use `cabal run <benchmark> -- --output ...`).

The `templates` directory contains a template file for use by Criterion: this extends
the HTML report to include the total number of times each benchmark was run and the
total amount of time spent running each benchmark.

### Tests

The directory `nofib/test` contains some tests for the nofib examples which
compare the result of evaluating the benchmarks as Haskell programs and as
Zerepoch Core programs.  Run these with `stack test zerepoch-benchmark` or
`cabal test zerepoch-benchmark`.