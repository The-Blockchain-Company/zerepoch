############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, rPackages
, haskell-nix
, agdaWithStdlib
, gitignore-nix
, z3
, R
, libsodium-vrf
, checkMaterialization
, compiler-nix-name
, enableHaskellProfiling
  # Whether to set the `defer-plugin-errors` flag on those packages that need
  # it. If set to true, we will also build the haddocks for those packages.
, deferPluginErrors
, actus-tests
}:
let
  r-packages = with rPackages; [ R tidyverse dplyr stringr MASS plotly shiny shinyjs purrr ];
  project = haskell-nix.cabalProject' ({ pkgs, ... }: {
    inherit compiler-nix-name;
    # This is incredibly difficult to get right, almost everything goes wrong, see https://github.com/The-Blockchain-Company/haskell.nix/issues/496
    src = let root = ../../../.; in
      haskell-nix.haskellLib.cleanSourceWith {
        filter = gitignore-nix.gitignoreFilter root;
        src = root;
        # Otherwise this depends on the name in the parent directory, which reduces caching, and is
        # particularly bad on Hercules, see https://github.com/hercules-ci/support/issues/40
        name = "zerepoch";
      };
    # These files need to be regenerated when you change the cabal files.
    # See ../CONTRIBUTING.doc for more information.
    # Unfortuntely, they are *not* constant across all possible systems, so in some circumstances we need different sets of files
    # At the moment, we only need one but conceivably we might need one for darwin in future.
    # See https://github.com/The-Blockchain-Company/nix-tools/issues/97
    materialized =
      if pkgs.stdenv.hostPlatform.isLinux then ./materialized-linux
      else if pkgs.stdenv.hostPlatform.isDarwin then ./materialized-darwin
      else if pkgs.stdenv.hostPlatform.isWindows then ./materialized-windows
      else builtins.error "Don't have materialized files for this platform";
    # If true, we check that the generated files are correct. Set in the CI so we don't make mistakes.
    inherit checkMaterialization;
    sha256map = {
      "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
      "https://github.com/shmish111/purescript-bridge.git"."6a92d7853ea514be8b70bab5e72077bf5a510596" = "13j64vv116in3c204qsl1v0ajphac9fqvsjp7x3zzfr7n7g61drb";
      "https://github.com/shmish111/servant-purescript.git"."a76104490499aa72d40c2790d10e9383e0dbde63" = "11nxxmi5bw66va7psvrgrw7b7n85fvqgfp58yva99w3v9q3a50v9";
      "https://github.com/The-Blockchain-Company/bcc-base"."46502694f6a9f0498f822068008b232b3837a9e9" = "04bvsvghkrjhfjb3phh0s5yfb37fishglrrlcwbvcv48y2in1dcz";
      "https://github.com/The-Blockchain-Company/bcc-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "06sdx5ndn2g722jhpicmg96vsrys89fl81k8290b3lr6b1b0w4m3";
      "https://github.com/The-Blockchain-Company/bcc-ledger-specs"."8efcfc755faae4db3a64ad45343235fce3ed5c47" = "13mj8nqk4jglyl96d6zm3dbjmx2qn5gwn06g7cmanxiwfkfm7bi1";
      "https://github.com/The-Blockchain-Company/bcc-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "02jddik1yw0222wd6q0vv10f7y8rdgrlqaiy83ph002f9kjx7mh6";
      "https://github.com/The-Blockchain-Company/shepards"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
      "https://github.com/The-Blockchain-Company/tbco-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" = "1il8fx3misp3650ryj368b3x95ksz01zz3x0z9k00807j93d0ka0";
      "https://github.com/The-Blockchain-Company/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
      "https://github.com/The-Blockchain-Company/shardagnostic-network"."6d00ff77f9bcd769fb6d7fd02216cec4e837bfcf" = "19dfhm9r1z00jqwpbnx7z0d58gpqsbwx4p96xlwwamd40hi3asn3";
      "https://github.com/The-Blockchain-Company/bcc-node.git"."8cf2b208c7708bd890c7e74d5b1d7c4167a3b40b" = "0akmzzf1gb8067knlzpbqdpkn3zrk5fm16icdzip44ilzwl5y2m0";
      "https://github.com/The-Blockchain-Company/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
      "https://github.com/The-Blockchain-Company/hedgehog-extras"."edf6945007177a638fbeb8802397f3a6f4e47c14" = "0wc7qzkc7j4ns2rz562h6qrx2f8xyq7yjcb7zidnj7f6j0pcd0i9";
    };
    # Configuration settings needed for cabal configure to work when cross compiling
    # for windows. We can't use `modules` for these as `modules` are only applied
    # after cabal has been configured.
    cabalProjectLocal = lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
      -- When cross compiling for windows we don't have a `ghc` package, so use
      -- the `zerepoch-ghc-stub` package instead.
      package zerepoch-tx-plugin
        flags: +use-ghc-stub

      -- Exlcude test that use `doctest`.  They will not work for windows
      -- cross compilation and `cabal` will not be able to make a plan.
      package simeon
        tests: False
      package prettyprinter-configurable
        tests: False
    '';
    modules = [
      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        packages = {
          # Things that need zerepoch-tx-plugin
          simeon.package.buildable = false; # Would also require libpq
          simeon-actus.package.buildable = false;
          simeon-dashboard-server.package.buildable = false;
          simeon-playground-server.package.buildable = false; # Would also require libpq
          simeon-symbolic.package.buildable = false;
          playground-common.package.buildable = false;
          zerepoch-benchmark.package.buildable = false;
          zerepoch-chain-index.package.buildable = false;
          zerepoch-contract.package.buildable = false;
          zerepoch-errors.package.buildable = false;
          zerepoch-ledger.package.buildable = false;
          zerepoch-pab.package.buildable = false;
          zerepoch-playground-server.package.buildable = false; # Would also require libpq
          zerepoch-use-cases.package.buildable = false;
          web-ghc.package.buildable = false;
          # Needs agda
          zerepoch-metatheory.package.buildable = false;
          # These need R
          zerepoch-core.components.benchmarks.cost-model-test.buildable = lib.mkForce false;
          zerepoch-core.components.benchmarks.update-cost-model.buildable = lib.mkForce false;
          # Windows build of libpq is marked as broken
          fake-pab.package.buildable = false;
        };
      })
      ({ pkgs, ... }:
        let
          # Add symlinks to the DLLs used by executable code to the `bin` directory
          # of the components with we are going to run.
          # We should try to find a way to automate this will in haskell.nix.
          symlinkDlls = ''
            ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll
            ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libgcc_s_seh-1.dll $out/bin/libgcc_s_seh-1.dll
            ln -s ${pkgs.buildPackages.gcc.cc}/x86_64-w64-mingw32/lib/libstdc++-6.dll $out/bin/libstdc++-6.dll
            ln -s ${pkgs.windows.mcfgthreads}/bin/mcfgthread-12.dll $out/bin/mcfgthread-12.dll
          '';
        in
        lib.mkIf (pkgs.stdenv.hostPlatform.isWindows) {
          packages = {
            # Add dll symlinks to the compoents we want to run.
            zerepoch-core.components.tests.zerepoch-core-test.postInstall = symlinkDlls;
            zerepoch-core.components.tests.zerepoch-ir-test.postInstall = symlinkDlls;
            zerepoch-core.components.tests.untyped-zerepoch-core-test.postInstall = symlinkDlls;
            zerepoch-ledger-api.components.tests.zerepoch-ledger-api-test.postInstall = symlinkDlls;

            # These three tests try to use `diff` and the following could be used to make the
            # linux version of diff available.  Unfortunately the paths passed to it are windows style.
            # zerepoch-core.components.tests.zerepoch-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
            # zerepoch-core.components.tests.zerepoch-ir-test.build-tools = [ pkgs.buildPackages.diffutils ];
            # zerepoch-core.components.tests.untyped-zerepoch-core-test.build-tools = [ pkgs.buildPackages.diffutils ];
            zerepoch-core.components.tests.zerepoch-core-test.buildable = lib.mkForce false;
            zerepoch-core.components.tests.zerepoch-ir-test.buildable = lib.mkForce false;
            zerepoch-core.components.tests.untyped-zerepoch-core-test.buildable = lib.mkForce false;
          };
        }
      )
      ({ pkgs, config, ... }: {
        packages = {
          # See https://github.com/The-Blockchain-Company/zerepoch/issues/1213 and
          # https://github.com/The-Blockchain-Company/zerepoch/pull/2865.
          simeon.doHaddock = deferPluginErrors;
          simeon.flags.defer-plugin-errors = deferPluginErrors;

          zerepoch-contract.doHaddock = deferPluginErrors;
          zerepoch-contract.flags.defer-plugin-errors = deferPluginErrors;

          zerepoch-use-cases.doHaddock = deferPluginErrors;
          zerepoch-use-cases.flags.defer-plugin-errors = deferPluginErrors;

          zerepoch-ledger.doHaddock = deferPluginErrors;
          zerepoch-ledger.flags.defer-plugin-errors = deferPluginErrors;

          # Packages we just don't want docs for
          zerepoch-benchmark.doHaddock = false;
          # FIXME: Haddock mysteriously gives a spurious missing-home-modules warning
          zerepoch-tx-plugin.doHaddock = false;

          # Fix missing executables on the paths of the test runners. This is arguably
          # a bug, and the fix is a bit of a hack.
          simeon.components.tests.simeon-test.preCheck = ''
            PATH=${lib.makeBinPath [ z3 ]}:$PATH
          '';
          # In this case we can just propagate the native dependencies for the build of the test executable,
          # which are actually set up right (we have a build-tool-depends on the executable we need)
          # I'm slightly surprised this works, hooray for laziness!
          zerepoch-metatheory.components.tests.test1.preCheck = ''
            PATH=${lib.makeBinPath project.hsPkgs.zerepoch-metatheory.components.tests.test1.executableToolDepends }:$PATH
          '';
          # FIXME: Somehow this is broken even with setting the path up as above
          zerepoch-metatheory.components.tests.test2.doCheck = false;
          # zerepoch-metatheory needs agda with the stdlib around for the custom setup
          # I can't figure out a way to apply this as a blanket change for all the components in the package, oh well
          zerepoch-metatheory.components.library.build-tools = [ agdaWithStdlib ];
          zerepoch-metatheory.components.exes.plc-agda.build-tools = [ agdaWithStdlib ];
          zerepoch-metatheory.components.tests.test1.build-tools = [ agdaWithStdlib ];
          zerepoch-metatheory.components.tests.test2.build-tools = [ agdaWithStdlib ];
          zerepoch-metatheory.components.tests.test3.build-tools = [ agdaWithStdlib ];

          # Relies on cabal-doctest, just turn it off in the Nix build
          prettyprinter-configurable.components.tests.prettyprinter-configurable-doctest.buildable = lib.mkForce false;

          zerepoch-core.components.benchmarks.update-cost-model = {
            build-tools = r-packages;
            # Seems to be broken on darwin for some reason
            platforms = lib.platforms.linux;
          };

          zerepoch-core.components.benchmarks.cost-model-test = {
            build-tools = r-packages;
            # Seems to be broken on darwin for some reason
            platforms = lib.platforms.linux;
          };

          simeon-actus.components.exes.simeon-shiny = {
            build-tools = r-packages;
            # Seems to be broken on darwin for some reason
            platforms = lib.platforms.linux;
          };

          # The simeon-actus tests depend on external data which is
          # provided from Nix (as niv dependency)
          simeon-actus.components.tests.simeon-actus-test.preCheck = ''
            export ACTUS_TEST_DATA_DIR=${actus-tests}/tests/
          '';


          # Note: The following two statements say that these tests should
          # _only_ run on linux. In actual fact we just don't want them
          # running on the 'mac-mini' instances, because these tests time out
          # there. In an ideal world this would be reflected here more
          # accurately.
          # TODO: Resolve this situation in a better way.
          simeon.components.tests.simeon-test-long-running = {
            platforms = lib.platforms.linux;
          };

          zerepoch-pab.components.tests.zerepoch-pab-test-full-long-running = {
            platforms = lib.platforms.linux;
          };


          # Broken due to warnings, unclear why the setting that fixes this for the build doesn't work here.
          tbco-monitoring.doHaddock = false;

          # Werror everything. This is a pain, see https://github.com/The-Blockchain-Company/haskell.nix/issues/519
          zerepoch-core.ghcOptions = [ "-Werror" ];
          simeon.ghcOptions = [ "-Werror" ];
          simeon-symbolic.ghcOptions = [ "-Werror" ];
          simeon-actus.ghcOptions = [ "-Werror" ];
          simeon-playground-server.ghcOptions = [ "-Werror" ];
          simeon-dashboard-server.ghcOptions = [ "-Werror" ];
          fake-pab.ghcOptions = [ "-Werror" ];
          playground-common.ghcOptions = [ "-Werror" ];
          # FIXME: has warnings
          #zerepoch-metatheory.package.ghcOptions = "-Werror";
          zerepoch-contract.ghcOptions = [ "-Werror" ];
          zerepoch-ledger.ghcOptions = [ "-Werror" ];
          zerepoch-ledger-api.ghcOptions = [ "-Werror" ];
          zerepoch-playground-server.ghcOptions = [ "-Werror" ];
          zerepoch-pab.ghcOptions = [ "-Werror" ];
          zerepoch-tx.ghcOptions = [ "-Werror" ];
          zerepoch-tx-plugin.ghcOptions = [ "-Werror" ];
          zerepoch-doc.ghcOptions = [ "-Werror" ];
          zerepoch-use-cases.ghcOptions = [ "-Werror" ];

          # External package settings

          inline-r.ghcOptions = [ "-XStandaloneKindSignatures" ];

          # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
          # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
          ieee.components.library.libs = lib.mkForce [ ];

          # See https://github.com/The-Blockchain-Company/tbco-nix/pull/488
          bcc-crypto-optimum.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
          bcc-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
        };
      })
    ] ++ lib.optional enableHaskellProfiling {
      enableLibraryProfiling = true;
      enableExecutableProfiling = true;
    };
  });

in
project
