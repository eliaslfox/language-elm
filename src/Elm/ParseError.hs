{-# LANGUAGE Strict #-}

module Elm.ParseError where

import Data.Monoid

data GenError
    = WarningList [String]
    | Error String
    deriving (Eq, Show)

instance Monoid GenError where
    mappend (Error str) _ = Error str
    mappend _ (Error str) = Error str
    mappend (WarningList a) (WarningList b) = WarningList $ a ++ b
    mempty = WarningList []
