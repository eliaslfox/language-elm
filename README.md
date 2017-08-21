# language-elm
A haskell library for generating elm source code from an ast.

## Usage
Install language-elm from stack
```
stack install language-elm
```

Import the libraries
```haskell
import Elm.Decleration
import Elm.Expression
import Elm.Import
import Elm.Program
import Elm.Type
```

Declare a program type
```haskell
program :: Program
program =
    Program
        "Tuple"
        (Select
            [ Item "first"
            , Item "second"
            , Item "mapFirst"
            , Item "mapSecond"
            ])
        []
        [ Dec
            "first"
            (TApp [TTuple2 (tvar "a1") (tvar "a2"), tvar "a1"])
            [Tuple2 (var "x") (Under)]
            (var "x")
        , Dec
            "second"
            (TApp [TTuple2 (tvar "a1") (tvar "a2"), tvar "a2"])
            [Tuple2 (Under) (var "y")]
            (var "y")
        , Dec
            "mapFirst"
            (TApp
                [ TApp [tvar "a", tvar "b"]
                , TTuple2 (tvar "a") (tvar "a2")
                , TTuple2 (tvar "b") (tvar "a2")
                ])
            [var "func", Tuple2 (var "x") (var "y")]
            (Tuple2 (App "func" [var "x"]) (var "y"))
        , Dec
            "mapSecond"
            (TApp
                [ TApp [tvar "a", tvar "b"]
                , TTuple2 (tvar "a1") (tvar "a")
                , TTuple2 (tvar "a1") (tvar "b")
                ])
            [var "func", Tuple2 (var "x") (var "y")]
            (Tuple2 (var "x") (App "func" [var "y"]))
        ]
```

Then render the program

```haskell
output :: String
output = renderProgram program
```

Which results in the following output

```elm
module Tuple exposing (first, second, mapFirst, mapSecond)

first :: ((a1, a2)) -> a1
first (x, _) = x

second :: ((a1, a2)) -> a2
second (_, y) = y

mapFirst :: (a -> b) -> ((a, a2)) -> ((b, a2))
mapFirst func (x, y) = (func x, y)

mapSecond :: (a -> b) -> ((a1, a)) -> ((a1, b))
mapSecond func (x, y) = (x, func y)
```

