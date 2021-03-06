{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "simeon-marketplace-client"
, dependencies =
  [ "aff-promise"
  , "avar"
  , "concurrent-queues"
  , "coroutines"
  , "debug"
  , "effect"
  , "halogen"
  , "node-fs"
  , "prelude"
  , "psci-support"
  , "remotedata"
  , "servant-support"
  , "test-unit"
  , "undefinable"
  , "unfoldable"
  , "uuid"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  , "web-common/**/*.purs"
  , "web-common-simeon/**/*.purs"
  ]
}
