{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-bus"
  , "affjax"
  , "avar"
  , "console"
  , "debug"
  , "effect"
  , "functors"
  , "js-timers"
  , "lists"
  , "maybe"
  , "psci-support"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "pages/assets/js/pure/**/*.purs", "test/**/*.purs" ]
}
