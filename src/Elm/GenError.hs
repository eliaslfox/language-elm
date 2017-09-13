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

instance Monoid GenError where
    mappend (Error str) _                   = Error str
    mappend _ (Error str)                   = Error str
    mappend (WarningList a) (WarningList b) = WarningList $ a ++ b
    mempty = WarningList []
