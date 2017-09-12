{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

-- | Ast for expressing imports
module Elm.Import where

import           Protolude        hiding (empty, (<>))

import           Control.Monad    (mapM)
import           Data.Maybe
import           Data.String
import           Elm.Classes
import           Text.PrettyPrint

-- | Possible ways to expose an import
data ImportType
    = Everything
    | Select [ImportItem]

-- | Possible ways to expose a sub import
data ImportItem
    = Item String
    | ItemExposing String [String]
    | ItemEvery String

-- | A full import
data Import = Import String (Maybe String) (Maybe ImportType)

instance Generate ImportItem where
    generate item =
        case item of
            Item str ->
                return . text $ str

            ItemExposing str [] ->
                return $ text str <> "()"

            ItemExposing str exposedItems ->
                return $ text str <> (parens . hsep . punctuate "," . map text $ exposedItems)

            ItemEvery str ->
                return $ text str <> "(..)"

instance Generate ImportType where
    generate item =
        case item of
            Everything ->
                return "(..)"

            Select [] -> do
                return "()"

            Select items -> do
               docItems <- mapM generate items
               return $ parens . hsep . punctuate "," $ docItems


instance Generate Import where
    generate (Import name as exposing) = do
        let asDoc = Data.Maybe.fromMaybe empty $ fmap (\str -> "as" <+> text str) $ as
        exposingDoc <-
            case exposing of
                Nothing ->
                    return empty

                Just e -> do
                    docE <- generate e
                    return $ "exposing" <+> docE
        return $ "import" <+> text name <+> asDoc <+> exposingDoc
