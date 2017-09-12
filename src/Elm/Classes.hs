{-# OPTIONS_HADDOCK -prune #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}

-- | A series of helper classes to make writing an ast easier
module Elm.Classes where

import           Protolude            ()

import           Control.Monad.Writer
import           Elm.GenError
import           Text.PrettyPrint

class Generate a where
    generate :: a -> Writer GenError Doc
