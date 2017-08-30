{-# LANGUAGE Safe #-}

-- | A series of helper classes to make writing an ast easier
module Elm.Classes where

import Control.Monad.Writer
import Text.PrettyPrint
import Elm.ParseError

-- | Represents a state of selecting every
class HasEvery a where
    every :: a

-- | Select an item
class Select a where
    select :: String -> a

-- | Select an item and items it contains
class SubSelect a where
    subSelect :: String -> [String] -> a

-- | Represents a variable
class Var a where
    var :: String -> a

-- | Represents function application
class App a where
    app :: String -> [a] -> a


class Generate a where
    generate :: a -> Writer GenError Doc
