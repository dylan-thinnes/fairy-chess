{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "css"
    , "effect"
    , "halogen"
    , "halogen-css"
    , "matryoshka"
    , "monad-loops"
    , "profunctor-lenses"
    , "psci-support"
    , "sized-vectors"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
