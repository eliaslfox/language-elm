{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}
{-# LANGUAGE Strict            #-}

module Elm.GenError (GenError(..)) where

import           Protolude

import           Data.String

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
