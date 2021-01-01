{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-swerve"
, dependencies =
  [ "arrays"
  , "b64"
  , "console"
  , "debugged"
  , "effect"
  , "form-urlencoded"
  , "heterogeneous"
  , "http-media"
  , "http-types"
  , "node-http"
  , "psci-support"
  , "simple-json"
  , "wai"
  , "warp"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
