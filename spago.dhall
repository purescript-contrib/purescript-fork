{ name = "fork"
, dependencies =
  [ "aff", "console", "effect", "prelude", "psci-support", "transformers" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
