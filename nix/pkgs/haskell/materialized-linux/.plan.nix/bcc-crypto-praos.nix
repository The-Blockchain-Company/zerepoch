{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { development = false; external-libsodium-vrf = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "bcc-crypto-praos"; version = "2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 The Blockchain Co.";
      maintainer = "operations@bcccoin.io";
      author = "The Blockchain Co.";
      homepage = "";
      url = "";
      synopsis = "Crypto primitives from libsodium";
      description = "VRF (and KES, tba) primitives from libsodium.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "cbits/crypto_vrf.h"
        "cbits/crypto_vrf_ietfdraft03.h"
        "cbits/vrf_ietfdraft03.h"
        "cbits/private/common.h"
        "cbits/private/quirks.h"
        "cbits/private/ed25519_ref10.h"
        "cbits/private/ed25519_ref10_fe_25_5.h"
        "cbits/private/ed25519_ref10_fe_51.h"
        "cbits/private/fe_25_5/constants.h"
        "cbits/private/fe_25_5/base.h"
        "cbits/private/fe_25_5/base2.h"
        "cbits/private/fe_25_5/fe.h"
        "cbits/private/fe_51/constants.h"
        "cbits/private/fe_51/base.h"
        "cbits/private/fe_51/base2.h"
        "cbits/private/fe_51/fe.h"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."bcc-binary" or (errorHandler.buildDepError "bcc-binary"))
          (hsPkgs."bcc-crypto-class" or (errorHandler.buildDepError "bcc-crypto-class"))
          (hsPkgs."bcc-prelude" or (errorHandler.buildDepError "bcc-prelude"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"))
          ];
        buildable = true;
        modules = [ "Bcc/Crypto/VRF/Praos" ];
        cSources = (pkgs.lib).optionals (!flags.external-libsodium-vrf) [
          "cbits/crypto_vrf.c"
          "cbits/convert.c"
          "cbits/keypair.c"
          "cbits/prove.c"
          "cbits/verify.c"
          "cbits/vrf.c"
          "cbits/private/ed25519_ref10.c"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "4";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "4";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/bcc-crypto-praos; echo source root reset to \$sourceRoot";
    }