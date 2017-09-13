{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Trustworthy       #-}
module Elm
    (
    -- * Expressions
    Expr
    , bool
    , string
    , int
    , float
    , under
    , var
    , app
    , list
    , op
    , let_
    , case_
    , parens
    -- * Types
    , Type
    , tvar
    , tparam
    , tparams
    , tapp
    , tunit
    , ttuple
    , trecord
    , trecordParam
    -- * Declerations
    , Dec
    , decVariable
    , decFunction
    , decType
    , decTypeAlias
    -- * Imports
    , Import
    , ImportExpr
    , ImportItem
    , select
    , subSelect
    , subSelectEvery
    , importSome
    , importEvery
    , import_
    -- * Module
    , Module
    , module_
    -- * Generation
    , renderModule
    , render
    , genStr
    )
    where

-- Please note that doctest is very specefic about spacing
-- so chaning spacing in the examples may break tests
import           Prelude              (error)
import           Protolude            hiding (bool, list)

import           Control.Monad.Writer
import           Data.String
import           Elm.Classes
import qualified Elm.Decleration
import qualified Elm.Expression
import           Elm.GenError         (GenError (WarningList))
import qualified Elm.Import
import qualified Elm.Program
import qualified Elm.Type             (TypeDec (..))
import qualified Text.PrettyPrint

type Expr = Elm.Expression.Expr
type Type = Elm.Type.TypeDec
type Dec = Elm.Decleration.Dec
type Import = Elm.Import.Import
type ImportExpr = Elm.Import.ImportType
type ImportItem = Elm.Import.ImportItem
type Module = Elm.Program.Program

-- | A boolean literal
bool :: Bool -> Expr
bool = Elm.Expression.Bool

-- | A string literal
string :: String -> Expr
string = Elm.Expression.Str

-- | An integer literal
int :: Int -> Expr
int = Elm.Expression.Int

-- | A float literal
float :: Float -> Expr
float = Elm.Expression.Float

-- | An _ literal
under :: Expr
under = Elm.Expression.Under

-- | A variable
var :: String -> Expr
var = Elm.Expression.Var

-- | Function application
--
-- >>> genStr (app [var "a", var "b", var "c"])
-- "a b c"
app :: [Expr] -> Expr
app = Elm.Expression.App

-- | A list literal
list :: [Expr] -> Expr
list = Elm.Expression.List

-- | Apply an operator to two sub expressions
--
-- >>> genStr (op "+" (int 5) (int 6))
-- "5 + 6"
op :: String -> Expr -> Expr -> Expr
op = Elm.Expression.Op

-- | A let...in block
--
-- >>> putStrLn $ genStr (let_ (var "a") [(var "a", int 5)])
-- let
--     a = 5
-- in
--     a
let_ :: Expr -> [(Expr, Expr)] -> Expr
let_ = Elm.Expression.Let

-- | A case...of block
--
-- >>> :{
--  putStrLn $ genStr
--      (case_ (var "m")
--          [ (app [var "Just", var "x"], var "x")
--          , (var "Nothing", var "default")
--          ])
-- :}
-- case m of
--     Just x ->
--         x
-- <BLANKLINE>
--     Nothing ->
--         default
case_ :: Expr -> [(Expr, Expr)] -> Expr
case_ = Elm.Expression.Case

-- | Wrap a sub expression in parens
parens :: Expr -> Expr
parens = Elm.Expression.Parens

-- * Types
-- Functions for generating type signatures


-- | A type or type variable
--
-- >>> genStr (tvar "Nothing")
-- "Nothing"
--
-- >>> genStr (tvar "a")
-- "a"
--
tvar :: String -> Type
tvar name = Elm.Type.Params name []

-- | A type with a single paramater
--
-- >>> genStr (tparam "Just" (tvar "a"))
-- "Just a"
--
tparam :: String -> Type -> Type
tparam name type_ = Elm.Type.Params name [type_]

-- | A type with multiple paramaters
--
-- >>> genStr (tparams "Result" [tvar "String", tvar "Int"])
-- "Result String Int"
--
tparams :: String -> [Type] -> Type
tparams = Elm.Type.Params

-- | A zero item tuple type
--
-- >>> genStr tunit
-- "()"
--
tunit :: Type
tunit = Elm.Type.TTuple []

-- | A multiple item tuple
--
-- >>> genStr (ttuple [tvar "a", tvar "b"])
-- "(a, b)"
--
ttuple :: [Type] -> Type
ttuple = Elm.Type.TTuple

-- | Type application
--
-- >>> genStr (tapp [tvar "a", tvar "b", tvar "c"])
-- "a -> b -> c"
--
tapp :: [Type] -> Type
tapp = Elm.Type.TApp


-- | A record type
--
-- >>> genStr (trecord [("a", tvar "Int"), ("b", tvar "String")])
-- "{ a : Int, b : String }"
--
trecord :: [(String, Type)] -> Type
trecord = Elm.Type.TRecord Nothing

-- | A paramaterized record type
--
-- >>> genStr (trecordParam "a" [("b", tvar "Int")])
-- "{ a | b : Int }"
--
trecordParam :: String -> [(String, Type)] -> Type
trecordParam = Elm.Type.TRecord . Just

-- Declare variables, functions, types, and type aliases

-- | Declare a variable
decVariable :: String -- ^ The variable name
    -> Type -- ^ The variable's type
    -> Expr -- ^ The variable's value
    -> Dec
decVariable name type_ expr = Elm.Decleration.Dec  name type_ [] expr

-- | Declare a function
decFunction :: String  -- ^ The function name
    -> Type  -- ^ The function's type
    -> [Expr] -- ^ The fuction's paramaters
    -> Expr  -- ^ The function's value
    -> Dec
decFunction = Elm.Decleration.Dec

-- | Declare a type
decType :: String -- ^ The type name
    -> [String] -- ^ The type's type paramaters
    -> [(String, [Type])] -- ^ The type's constructors
    -> Dec
decType = Elm.Decleration.DecType

-- | Declare a type alias
decTypeAlias :: String  -- ^ The type alias' name
    -> [String]  -- ^ The type alias's type paramaters
    -> Type  -- ^ The type alias's type
    -> Dec
decTypeAlias = Elm.Decleration.DecTypeAlias

-- | Import an item
select :: String -> ImportItem
select = Elm.Import.Item

-- | Import an item and some constructors
subSelect :: String -> [String] -> ImportItem
subSelect = Elm.Import.ItemExposing

-- | Import an item and all constructors
subSelectEvery :: String -> ImportItem
subSelectEvery = Elm.Import.ItemEvery

-- | Import all exports of a module
importSome :: [ImportItem] -> ImportExpr
importSome = Elm.Import.Select

-- | Import some exports of a module
importEvery :: ImportExpr
importEvery = Elm.Import.Everything

-- | Import a module
import_ :: String  -- ^ The name of the module to import
    -> (Maybe String)  -- ^ A possible alias to import the module as
    -> (Maybe ImportExpr) -- ^ A possible set of items to expose
    -> Import
import_ = Elm.Import.Import

-- | Generate a full module
module_ :: String -- ^ The module name
    -> ImportExpr -- ^ The module exports
    -> [Import] -- ^ The module imports
    -> [Dec] -- ^ The module decleration
    -> Module
module_ = Elm.Program.Program

-- | Render a module, returning possible errors or warnings
renderModule :: Module -> (String, GenError)
renderModule = Elm.Program.renderProgram

-- | Render a module, throwing an error if there is an error or warnings
render :: Module -> String
render module' =
    let
        (str, err) = renderModule module'
    in
        if err == WarningList [] then
            str
        else
            error . show $ err


genStr :: (Generate a) => a -> String
genStr g =
    let
        (str, err) = runWriter . generate $ g
    in
        if err == WarningList [] then
            Text.PrettyPrint.render str
        else
            error . show $ err
