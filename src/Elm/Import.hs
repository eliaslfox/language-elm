{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE Safe #-}

-- | Ast for expressing imports
module Elm.Import where

import Text.PrettyPrint
import Elm.Classes

-- | Possible ways to expose an import
data ImportType
    = Everything
    | Select [ImportItem]
    | ExposeNothing

instance HasEvery ImportType where
    every = Everything

-- | Possible ways to expose a sub import
data ImportItem 
    = Item String
    | ItemExposing String [String]
    | ItemEvery String

instance Select ImportItem where
    select = Item

instance SubSelect ImportItem where
    subSelect = ItemExposing

-- | A full import
data Import = Import String (Maybe String) ImportType

instance Select Import where
    select module_ = Import module_ Nothing ExposeNothing

instance SubSelect Import where
    subSelect module_ items = Import module_ Nothing $ Select $ map Item items

docItem :: ImportItem -> Doc
docItem item =
    case item of
        Item str ->
            text str

        ItemExposing name exposes ->
            text name <> (parens . hsep . punctuate (text ",") . map text $ exposes)

        ItemEvery name ->
            text name <> text "(..)"

exposingDoc :: ImportType -> Doc
exposingDoc importType =
    case importType of
        Everything ->
            text "exposing (..)"

        ExposeNothing ->
            empty

        Select imports ->
            text "exposing" <+> (parens . hsep . punctuate (text ",") . map docItem $ imports)

toDocI :: Import -> Doc
toDocI (Import name as exposing) =
    text "import" <+> text name <+> asDoc <+> exposingDoc exposing

    where
        asDoc =
            case as of
                Nothing ->
                    empty

                Just str ->
                    text "as" <+> text str
            
                

