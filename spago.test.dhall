let conf = ./spago.dhall

let dependencies = [ "psci-support", "spec", "partial" ]

let sources = [ "test/**/*.purs" ]

in conf //
  { dependencies = conf.dependencies # dependencies
  , sources = conf.sources # sources
  }