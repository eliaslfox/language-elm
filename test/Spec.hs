module Main where

import Test.HUnit
import Expression
import Type
import Decleration
import Text.PrettyPrint hiding (Str)

testRender ast =
    (render . toDoc $ ast) ++ "\n"

testRenderDec ast =
    (render . toDocD $ ast) ++ "\n"

testApp =
        [ do
            assertEqual "No arguments" (text "f") (toDoc $ App "f" [])
        , do
            assertEqual "single var" (text "f a") (toDoc $ App "f" [App "a" []])
            assertEqual "two vars" (text "f a b") (toDoc $ App "f" [App "a" [], App "b" []])
        , do
            assertEqual "parens" (text "f (a b)") (toDoc $ App "f" [App "a" [App "b" []]])
        ]

testCase =
    [ do
        text <- readFile "test/case1.txt"
        let
            ast =
                Case (App "color" [])
                    [ (App "Red" [], Str "red")
                    , (App "Blue" [], Str "blue")
                    , (App "Green" [], Str "green")
                    ]
        assertEqual "colors" text $ testRender ast
    , do
        text <- readFile "test/case2.txt"
        let
            ast =
                Case (App "m" [])
                    [ (App "Just" [var "data"], var "data")
                    , (var "Nothing", Str "")
                    ]
        assertEqual "maybe" text $ testRender ast
    , do
        text <- readFile "test/case3.txt"
        let
            ast =
                Case (Tuple2 (var "a") (var "b"))
                    [ (Tuple2 (Int  1) (Int 2), BoolTrue)
                    , (Under, BoolFalse)
                    ]
        assertEqual "tuple" text $ testRender ast 
    , do
        text <- readFile "test/case4.txt"
        let
            ast =
                Case (App "f" [var "x"])
                    [ (BoolTrue, Str "true")
                    , (BoolFalse, Str "false")
                    ]

        assertEqual "application" text $ testRender ast
    , do
        text <- readFile "test/case5.txt"
        let
            ast =
                Case (var "b")
                    [ (BoolTrue,
                        Let (Str "true")
                            [ (Under, App "Debug.log" [Str "logging", Str "its true"]) 
                            ])
                    , (BoolFalse,
                        Let (Str "false")
                            [ (Under, App "Debug.log" [Str "logging", Str "its false"]) 
                            ])
                    ]

        assertEqual "let in case" text $ testRender ast
    ]

testLet =
    [ do
        text <- readFile "test/let1.txt"
        let
            ast =
                Let (var "a")
                    [ (var "a", Int 5) ]

        assertEqual "application" text $ testRender ast 
    , do
        text <- readFile "test/let2.txt"
        let
            ast =
                Let (App "f" [var "a", var "b"])
                    [ (var "a", Int 5)
                    , (var "b", Int 6)
                    ]
        assertEqual "more variables" text $ testRender ast
    ]

testOp =
    [ do
        assertEqual "plus" (text "a + b") (toDoc $ Op "+" (var "a") (var "b"))
        assertEqual "compose" (text "a <<< b") (toDoc $ Op "<<<" (var "a") (var "b"))
        assertEqual "longer names" (text "abc >>> def") 
            (toDoc $ Op ">>>" (var "abc") (var "def"))
        assertEqual "parens" (text "(f a) + (g b)")
            (toDoc $ (Op "+" (App "f" [var "a"]) (App "g" [var "b"])))
    ]

testList =
    [ do
        assertEqual "empty" (text "[]") (toDoc $ List [])
    , do
        assertEqual "some stuff" (text "[1, 2, 3]")
            (toDoc $ List [Int 1, Int 2, Int 3])
    ]

testType =
    [ do
        assertEqual "simple" (text "a") (toDocT $ tvar "a")
        assertEqual "param" (text "Maybe a") (toDocT $ Params "Maybe" [tvar "a"])
        assertEqual "multi param" (text "Result String Int")
            (toDocT $ Params "Result" [tvar "String", tvar "Int"])
    , do
        assertEqual "app" (text "a -> b")
            (toDocT $ TApp [tvar "a", tvar "b"])
        assertEqual "more app" (text "a -> b -> c")
            (toDocT $ TApp [tvar "a", tvar "b", tvar "c"])
        assertEqual "nested app" (text "(a -> b) -> a -> b")
            (toDocT $ TApp [TApp [tvar "a", tvar "b"], tvar "a", tvar "b"])
    , do
        assertEqual "more nested stuff" (text "Maybe (a -> b) String")
            (toDocT $ Params "Maybe" [TApp [tvar "a", tvar "b"], tvar "String"])
        assertEqual "alternate nesting" (text "Maybe (Maybe a)")
            (toDocT $ Params "Maybe" [Params "Maybe" [tvar "a"]])
    ]

testDec =
    [ do
        text <- readFile "test/dec.txt"
        let
            ast =
                Dec
                    "withDefault"
                    (TApp [tvar "a", Params "Maybe" [tvar "a"], tvar "a"])
                    [var "default", var "maybe"]
                    (Case 
                        (var "maybe")
                        [ (App "Just" [var "data"], var "data")
                        , (var "Nothing", var "default")
                        ])
        assertEqual "test" text $ testRenderDec ast
    ]

testDecType =
    [ do
        assertEqual "what even goes here?" (text "type User = User String")
            (toDocD $ DecType "User" [] [("User", [tvar "String"])])
        assertEqual "que??" (text "type Color = Red | Blue | Green")
            (toDocD $ DecType "Color" [] [("Red", []), ("Blue", []), ("Green", [])])
        assertEqual ":(" (text "type Data = DataA (Maybe String) | DataB (String -> Int)")
            (toDocD $ DecType "Data" []
                [ ("DataA", [Params "Maybe" [tvar "String"]])
                , ("DataB", [TApp [tvar "String", tvar "Int"]])
                ])
        assertEqual "noh" (text "type Maybe a = Nothing | Just a")
            (toDocD $ DecType "Maybe" ["a"]
                [ ("Nothing", [])
                , ("Just", [tvar "a"])
                ])
    ]

testDecTypeAlias =
    [ do
        assertEqual "this stuff" (text "type alias Model = Int")
            (toDocD $ DecTypeAlias "Model" [] (tvar "Int"))
        assertEqual "dubple" (text "type alias Duple a = (a, a)")
            (toDocD $ DecTypeAlias "Duple" ["a"] $ TTuple2 (tvar "a") (tvar "a"))
    ]

tests = 
    TestList 
        [ "test var" ~: testApp
        , "test case" ~: testCase
        , "test let" ~: testLet
        , "test op" ~: testOp
        , "test list" ~: testList
        , "test type" ~: testType
        , "test dec type alias" ~: testDecTypeAlias
        , "test dec type" ~: testDecType
        , "test dec" ~: testDec
        ]

main :: IO Counts
main = runTestTT tests
