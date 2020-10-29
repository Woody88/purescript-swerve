{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-swerve"
, dependencies =
  [ "console"
  , "effect"
  , "form-urlencoded"
  , "http-media"
  , "http-types"
  , "media-types"
  , "psci-support"
  , "simple-json"
  , "typelevel-prelude"
  , "wai"
  , "warp"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
