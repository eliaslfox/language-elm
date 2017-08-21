{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Expression
import Type
import Decleration
import Import
import Program
import Text.PrettyPrint hiding (Str)
import Control.Monad 
import Data.String.Utils

assertString :: String -> String -> String -> Assertion 
assertString preface expected actual =
  unless (actual == expected) (assertFailure msg)
   where msg = (if null preface then "" else preface ++ "\n") ++
                "expected:\n\n" ++ expected ++ "\n but got:\n\n" ++ actual ++
                "\n" ++ (show . length $ expected) ++ " - "
                ++ (show . length $ actual)

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

testRecord =
    [ do
        assertEqual "empty" (text "{}") (toDoc $ Record Nothing [])
        assertEqual "no sets" (text "a") (toDoc $ Record (Just $ var "a") [])
        assertEqual "some stuff"
            (text "{ a | b = 5 }")
            (toDoc $ Record (Just $ var "a") [("b", Int 5)])
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
    , do
        assertEqual "record types" (text "{}") (toDocT $ TRecord Nothing [])
        assertEqual "memier record types"
            (text "{ a : Int, b : Int }")
            (toDocT $ TRecord Nothing [("a", tvar "Int"), ("b", tvar "Int")])
        assertEqual "the dankest rarest memes"
            (text "{ a | b : Int }")
            (toDocT $ TRecord (Just "a") [("b", tvar "Int")])
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

testImport =
    [ do
        assertEqual "wut" (text "import List") (toDocI $ Import "List" Nothing ExposeNothing)
        assertEqual "wut" (text "import List as L") (toDocI $ Import "List" (Just "L") ExposeNothing)
        assertEqual "wut" (text "import List exposing (map)") (toDocI $ Import "List" Nothing $ Select [Item "map"])
        assertEqual "wut" (text "import List as L exposing (map)")
            (toDocI $ Import "List" (Just "L") $ Select [Item "map"])
        assertEqual "wut" (text "import List exposing (List(..))")
            (toDocI $ Import "List" Nothing $ Select [ItemEvery "List"])
        assertEqual "wut" (text "import List exposing (List(Cons))")
            (toDocI $ Import "List" Nothing $ Select [ItemExposing "List" ["Cons"]])
    ]

testProgram =
    [ do
        file <- readFile "test/program1.elm"

        let
            ast =
                Program
                    "Maybe"
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
                   [ DecType "Maybe" ["a"] [ ("Nothing", []), ("Just", [tvar "a"]) ]
                   , Dec "withDefault" 
                        (TApp [tvar "a", Params "Maybe" [tvar "a"], tvar "a"])
                        [var "default", var "maybe"] 
                        (Case (var "maybe")
                        [ (App "Just" [var "value"], var "value")
                        , (var "Nothing", var "default")
                        ])
                   , Dec "map"
                        (TApp 
                            [ TApp [tvar "a", tvar "b"]
                            , Params "Maybe" [tvar "a"]
                            , Params "Maybe" [tvar "b"]
                            ])
                        [var "f", var "maybe"]
                        (Case (var "maybe")
                            [ (App "Just" [var "value"], App "Just" [App "f" [var "value"]])
                            , (var "Nothing", var "Nothing")
                            ])
                    , Dec "map2"
                        (TApp
                            [ TApp [tvar "a", tvar "b", tvar "value"]
                            , Params "Maybe" [tvar "a"]
                            , Params "Maybe" [tvar "b"]
                            , Params "Maybe" [tvar "value"]
                            ])
                        [var "func", var "ma", var "mb"]
                        (Case (Tuple2 (var "ma") (var "mb"))
                            [ (Tuple2 (App "Just" [var "a"]) (App "Just" [var "b"])
                                , App "Just" [App "func" [var "a", var "b"]])
                            , (var "_", var "Nothing")
                            ])
                   , Dec "map3"
                        (TApp
                            [ TApp 
                                [ tvar "a"
                                , tvar "b"
                                , tvar "c"
                                , tvar "value"
                                ]
                            , Params "Maybe" [tvar "a"]
                            , Params "Maybe" [tvar "b"]
                            , Params "Maybe" [tvar "c"]
                            , Params "Maybe" [tvar "value"]
                            ])
                        [var "func", var "ma", var "mb", var "mc"]
                        (Case (Tuple3 (var "ma") (var "mb") (var "mc"))
                            [ (Tuple3
                                (App "Just" [var "a"])
                                (App "Just" [var "b"])
                                (App "Just" [var "c"]),
                              (App "Just" [App "func" [var "a", var "b", var "c"]]))
                            , (var "_", var "Nothing")
                            ]
                        )
                        

                   ]
        Main.assertString "meh" file $ renderProgram ast
    ]

tests = 
    TestList 
        [ "test var" ~: testApp
        , "test case" ~: testCase
        , "test let" ~: testLet
        , "test op" ~: testOp
        , "test list" ~: testList
        , "test record" ~: testRecord
        , "test type" ~: testType
        , "test dec type alias" ~: testDecTypeAlias
        , "test dec type" ~: testDecType
        , "test dec" ~: testDec
        , "test import" ~: testImport
        , "test program" ~: testProgram
        ]

main :: IO Counts
main = runTestTT tests
