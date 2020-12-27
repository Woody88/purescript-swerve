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
  , "http-media"
  , "http-types"
  , "psci-support"
  , "simple-json"
  , "node-http"
  , "wai"
  , "warp"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
