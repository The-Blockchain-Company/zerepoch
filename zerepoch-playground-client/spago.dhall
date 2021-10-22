{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "zerepoch-playground-client"
, dependencies =
  [ "prelude"
  , "aff"
  , "aff-promise"
  , "bigints"
  , "concurrent-queues"
  , "console"
  , "coroutines"
  , "aff-coroutines"
  , "debug"
  , "effect"
  , "halogen"
  , "matryoshka"
  , "node-fs"
  , "numerics"
  , "parsing"
  , "argonaut-codecs"
  , "foreign-generic"
  , "psci-support"
  , "remotedata"
  , "servant-support"
  , "test-unit"
  , "undefinable"
  , "uuid"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  , "generated/**/*.purs"
  , "web-common/**/*.purs"
  , "web-common-zerepoch/**/*.purs"
  , "web-common-playground/**/*.purs"
  ]
}
