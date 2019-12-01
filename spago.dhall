{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "halogen"
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
