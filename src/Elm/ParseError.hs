{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}
{-# LANGUAGE Strict            #-}

module Elm.ParseError where

import           Protolude

import           Data.String

data GenError
    = WarningList [String]
    | Error String
    deriving (Eq, Show)

instance Monoid GenError where
    mappend (Error str) _                   = Error str
    mappend _ (Error str)                   = Error str
    mappend (WarningList a) (WarningList b) = WarningList $ a ++ b
    mempty = WarningList []
