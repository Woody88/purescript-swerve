let conf = ./spago.dhall 
in conf // {
    sources = [ "examples/**/*.purs" ],
    dependencies = conf.dependencies # [ "warp" ]
}