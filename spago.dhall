{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
  , "form-urlencoded"
  , "heterogeneous"
  , "http-media"
  , "http-types"
  , "media-types"
  , "psci-support"
  , "record-format"
  , "simple-json"
  , "typelevel-prelude"
  , "wai"
  , "warp"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
