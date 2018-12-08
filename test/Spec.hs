{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Writer
import           Elm.Classes
import           Elm.Declaration
import           Elm.Expression
import           Elm.GenError
import           Elm.Import
import           Elm.Program
import           Elm.Type
import           Renderer
import           Test.Hspec
import           Text.PrettyPrint     hiding (Str, render)

main = hspec $ do
    describe "Setup" $ do
        it "Should work" $ do
            5 `shouldBe` 5
    describe "Expression" $ do
        describe "Bool" $ do
            it "Should render true properly" $ do
                let expr = Bool True
                renderExpr expr `shouldBe` "True"
            it "Should render false properly" $ do
                let expr = Bool False
                renderExpr expr `shouldBe` "False"
        describe "Int" $ do
            it "Should work" $ do
                renderExpr (Int 5) `shouldBe` "5"
        describe "Float" $ do
            it "Should work" $ do
                renderExpr (Float 1.2) `shouldBe` "1.2"
        describe "String" $ do
            it "Should work" $ do
                renderExpr (Str "hello") `shouldBe` "\"hello\""
        describe "Tuple" $ do
            it "Should work for 0 tuples" $ do
                renderExpr (Tuple []) `shouldBe` "()"
            it "Should work for larger tuples" $ do
                renderExpr (Tuple [Int 5, Int 6]) `shouldBe` "( 5, 6 )"
                renderExpr (Tuple [Int 2, Float 2.5]) `shouldBe` "( 2, 2.5 )"
        describe "Var" $ do
            it "Should work" $ do
                renderExpr (Var "a") `shouldBe` "a"
                renderExpr (Var "abc") `shouldBe` "abc"
            it "Should work with overloadedstrings" $ do
                renderExpr "a" `shouldBe` "a"
                renderExpr "abc" `shouldBe` "abc"
            it "Should error on an empty string" $ do
                (runWriter . generate $ Var "") `shouldBe` (text "", Error "An empty string is not a valid variable name")
            it "Should wrap operators in parens" $ do
                pending
            it "Should only allow operators with valid characters" $ do
                pending
            it "Should only allow variables with valid characters" $ do
                pending
        describe "Under" $ do
            it "Does this I guess" $ do
                renderExpr Under `shouldBe` "_"
        describe "List" $ do
            it "Should work for empty lists" $ do
                renderExpr (List []) `shouldBe` "[]"
            it "Should work for single item lists" $ do
                renderExpr (List ["a"]) `shouldBe` "[a]"
            it "Should work for multi item lists" $ do
                renderExpr (List ["a", "b", "c"]) `shouldBe` "[a, b, c]"
            it "Should work with overloaded lists" $ do
                renderExpr (List ["a", "b", "c"]) `shouldBe` "[a, b, c]"
        describe "Function Application" $ do
            it "Should work for 0 params" $ do
                renderExpr (App ["a"]) `shouldBe` "a"
            it "Should work for multiple params" $ do
                renderExpr (App ["a", "b", "c"]) `shouldBe` "a b c"
                renderExpr (App ["a", Int 5, "b", Int 7]) `shouldBe` "a 5 b 7"
            it "Should nest properly" $ do
                renderExpr (App ["a", App ["b", "c"]]) `shouldBe` "a (b c)"
        describe "Inline operators" $ do
            it "Should work" $ do
                renderExpr (Op "+" "a" "b") `shouldBe` "a + b"
            it "Should error on invalid operator characters" $ do
                pending
        describe "Record" $ do
            it "Should work when empty" $ do
                renderExpr (Record Nothing []) `shouldBe` "{}"
            it "Should work with key value pairs" $ do
                renderExpr (Record Nothing [("a", Int 5), ("b", Int 6)])
                    `shouldBe` "{ a = 5, b = 6 }"
            it "Should allow updates" $ do
                renderExpr (Record (Just "a") [("b", Int 5)])
                    `shouldBe` "{ a | b = 5 }"
            it "Should warn here" $ do
                (runWriter . generate $ Record (Just "data") [])
                    `shouldBe` ("data", WarningList ["Trying to update record data with no changed fields"])
            it "Should error here" $ do
                (runWriter . generate $ Record (Just $ App ["f", Int 5]) [])
                    `shouldBe` ("", Error "You are unable to update a record with a non constant")
        describe "Let" $ do
            it "Should work with one dec" $ do
                text <- liftIO . readFile $ "test/spec/let1.txt"
                let
                    ast = Let "a" [("a", Int 5)]
                renderExpr ast ++ "\n" `shouldBe` text
            it "Should work with more than one dec" $ do
                text <- liftIO . readFile $ "test/spec/let2.txt"
                let
                    ast = Let (Op "+" "a" "b") [("a", Int 5), ("b", Int 6)]
                renderExpr ast ++ "\n" `shouldBe` text
            it "Should error with zero declerations" $ do
                (runWriter . generate $ Let "a" [])
                    `shouldBe` ("", Error "Unable to create let expression with 0 bindings")
        describe "Case" $ do
            it "Should work" $ do
                text <- liftIO . readFile $ "test/spec/case1.txt"
                let
                    ast =
                        Case "m"
                            [(App ["Just", "x"], "x")
                            ,("Nothing", Int 0)
                            ]
                renderExpr ast ++ "\n" `shouldBe` text
            it "Should generate an error with no cases" $ do
                let
                    ast = Case "m" []
                (runWriter . generate $ ast)
                    `shouldBe` ("", Error "Unable to create case expression with 0 cases")

    describe "Type" $ do
        describe "Params" $ do
            it "Should work for without params" $ do
                render (Params "a" [])
                    `shouldBe` "a"
            it "Should work with params" $ do
                render (Params "a" [Params "b" []])
                    `shouldBe` "a b"
            it "Should put params in parens if needed" $ do
                render (Params "a" [Params "b" [Params "c" []]])
                    `shouldBe` "a (b c)"

        describe "TApp" $ do
            it "Should work" $ do
                render (TApp [Params "a" [], Params "b" []])
                    `shouldBe` "a -> b"

            it "Should put nested tapps in parens" $ do
                render (TApp [Params "a" [], TApp [Params "a" [], Params "b" []]])
                    `shouldBe` "a -> (a -> b)"

        describe "TTuple" $ do
            it "Should work for unit" $ do
                render (TTuple []) `shouldBe` "()"

            it "Should return the value and warn with a one item tuple" $ do
                let
                    ast =
                        TTuple [Params "a" []]
                (runWriter . generate $ ast)
                    `shouldBe` ("(a)", WarningList ["Attempt to create a one item tuple"])
            it "Should work for multiple item tuples" $ do
                render (TTuple [Params "a" [], Params "b" []])
                    `shouldBe` "(a, b)"

        describe "TRecord" $ do
            it "Should error when passed nothing" $ do
                let
                    ast =
                        TRecord Nothing []
                (runWriter . generate $ ast)
                    `shouldBe` ("", Error "Unable to create a record type with no base and no constraints")

            it "Should warn when there are no constraints" $ do
                let
                    ast =
                        TRecord (Just "a") []
                (runWriter . generate $ ast)
                    `shouldBe` ("a", WarningList ["You are creating a record type from a with no constraints"])
            it "Should work with no base" $ do
                let
                    ast =
                        TRecord Nothing
                            [ ("a", Params "Int" [])
                            , ("b", Params "Int" [])
                            ]
                render ast `shouldBe` "{ a : Int, b : Int }"

            it "Should work with a base" $ do
                let
                    ast =
                        TRecord (Just "a")
                            [ ("b", Params "Int" [])
                            , ("c", Params "Int" [])
                            ]
                render ast `shouldBe` "{ a | b : Int, c : Int }"
    describe "Import" $ do
        describe "ImportItem" $ do
            it "Should work for single items" $ do
                render (Item "Maybe")
                    `shouldBe` "Maybe"
            it "Should work for sub importing" $ do
                render (ItemExposing "Maybe" ["Just", "Nothing"])
                    `shouldBe` "Maybe(Just, Nothing)"
            it "Should working for sub importing everything" $ do
                render (ItemEvery "Maybe")
                    `shouldBe` "Maybe(..)"
        describe "ImportType" $ do
            it "Should properly select everything" $ do
                render (Everything) `shouldBe` "(..)"
            it "Should properly select only types" $ do
                render (Select []) `shouldBe` "()"
            it "Should properly select items" $ do
                render (Select [Item "Just", Item "withDefault"])
                    `shouldBe` "(Just, withDefault)"
                render (Select [ItemEvery "Just", Item "withDefault"])
                    `shouldBe` "(Just(..), withDefault)"
        describe "Import" $ do
            it "Should import stuff" $ do
                render (Import "List" Nothing Nothing)
                    `shouldBe` "import List"
            it "Should render with an alias" $ do
                render (Import "Maybe" (Just "M") Nothing)
                    `shouldBe` "import Maybe as M"
            it "Should expose from the imports" $ do
                render (Import "Maybe" Nothing $ Just $ Select [Item "withDefault"])
                    `shouldBe` "import Maybe exposing (withDefault)"
        describe "Dec" $ do
            it "Should work for simple delcerations" $ do
                let
                    ast =
                        Dec "add5"
                            (TApp ["Int", "Int"])
                            [Var "x"]
                            (Op "+" (Var "x") (Int 5))
                file <- readFile "test/spec/dec1.txt"
                render ast ++ "\n" `shouldBe` file
            it "Should work for more complex declerations" $ do
                let
                    ast =
                        Dec "withDefault"
                            (TApp ["a", Params "Maybe" ["a"], "a"])
                            ["default", "m"]
                            (Case "m"
                                [ (App ["Just", "x"], "x")
                                , ("Nothing", "default")
                                ])
                file <- readFile "test/spec/dec2.txt"
                render ast ++ "\n" `shouldBe` file
            it "Should properly handle type declerations" $ do
                -- \_(*_*)_/
                let
                    ast =
                        DecType "Maybe"
                        ["a"]
                        [ ("Nothing", [])
                        , ("Just", ["a"])
                        ]
                file <- readFile "test/spec/dec3.txt"
                render ast ++ "\n" `shouldBe` file
            it "Should properly handle type aliases" $ do
                let
                    ast =
                        DecTypeAlias "Model" ["a"] (Params "Maybe" ["a"])
                file <- readFile "test/spec/dec4.txt"
                render ast ++ "\n" `shouldBe` file
    describe "Program" $ do
        it "Should work" $ do
            let
                ast =
                    Program "Maybe"
                        (Select
                            [ ItemExposing "Maybe" ["Just", "Nothing"]
                            , Item "andThen"
                            , Item "map"
                            , Item "map2"
                            , Item "map3"
                            , Item "map4"
                            , Item "map5"
                            , Item "withDefault"
                            ])
                        []
                        [ DecType "Maybe" ["a"]
                            [ ("Nothing", [])
                            , ("Just", ["a"])
                            ]
                        , Dec "withDefault"
                            (TApp ["a", Params "Maybe" ["a"], "a"])
                            ["default", "maybe"]
                            (Case "maybe"
                                [ (App ["Just", "value"], "value")
                                , ("Nothing", "default")
                                ])
                        , Dec "map"
                            (TApp [TApp ["a", "b"], Params "Maybe" ["a"], Params "Maybe" ["b"]])
                            ["f", "maybe"]
                            (Case "maybe"
                                [ (App ["Just", "value"], App ["Just", App ["f", "value"]])
                                , ("Nothing", "Nothing")
                                ])
                        , Dec "map2"
                            (TApp
                                [ TApp ["a", "b", "value"]
                                , Params "Maybe" ["a"]
                                , Params "Maybe" ["b"]
                                , Params "Maybe" ["value"]
                                ])
                            ["func", "ma", "mb"]
                            (Case (Tuple ["ma", "mb"])
                                [ (Tuple [App ["Just", "a"], App ["Just", "b"]],
                                    App ["Just", App ["func", "a", "b"]])
                                , (Under, "Nothing")
                                ])
                        , Dec "map3"
                            (TApp
                                [ TApp ["a", "b", "c", "value"]
                                , Params "Maybe" ["a"]
                                , Params "Maybe" ["b"]
                                , Params "Maybe" ["c"]
                                , Params "Maybe" ["value"]
                                ])
                            ["func", "ma", "mb", "mc"]
                            (Case (Tuple ["ma", "mb", "mc"])
                                [ (Tuple
                                    [ App ["Just", "a"]
                                    , App ["Just", "b"]
                                    , App ["Just", "c"]
                                    ],
                                    App ["Just", App ["func", "a", "b", "c"]])
                                , (Under, "Nothing")
                                ])
                        ]
            file <- readFile "test/program1.elm"
            let (str, err) = renderProgram ast
            err `shouldBe` WarningList []
            str `shouldBe` file
