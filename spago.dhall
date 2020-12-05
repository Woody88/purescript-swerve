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
  , "media-types"
  , "partial"
  , "psci-support"
  , "simple-json"
  , "spec"
  , "stringutils"
  , "transformers"
  , "typelevel-prelude"
  , "wai"
  , "warp"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
