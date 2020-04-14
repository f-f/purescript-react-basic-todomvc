{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "purescript-react-basic-todomvc"
, dependencies =
  [ "effect"
  , "console"
  , "foreign-generic"
  , "foreign"
  , "simple-json"
  , "generics-rep"
  , "psci-support"
  , "react-basic"
  , "prelude"
  , "strings"
  , "debug"
  ]
, packages = ./packages.dhall
}
