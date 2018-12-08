{-# OPTIONS_GHC -Wall -Werror  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}
{-# LANGUAGE CPP               #-}

module Elm.GenError (GenError(..)) where

import           Protolude hiding ((<>))

import           Data.String

import Data.Semigroup (Semigroup, (<>))

-- | The error type
data GenError
    = WarningList [String]
    | Error String
    deriving (Eq, Show)

instance Semigroup GenError where
  (Error x) <> _ = Error x
  _ <> (Error y) = Error y
  (WarningList x) <> (WarningList y) = WarningList $ x <> y

instance Monoid GenError where
    mempty = WarningList []
    mappend = (<>)
