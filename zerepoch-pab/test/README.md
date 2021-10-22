# zerepoch-pab tests

There are two test suites in `zerepoch-pab.cabal`:

* `zerepoch-pab-test-full` has the main PAB tests (for `Zerepoch.PAB.Core` and related modules). It depends on `zerepoch-use-cases`.
* `zerepoch-pab-test-light` has some tests that don't depend on `zerepoch-use-cases`