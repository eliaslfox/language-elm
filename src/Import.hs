module Import where

import Text.PrettyPrint

data ImportType
    = Everything
    | Select [ImportItem]
    | ExposeNothing

data ImportItem 
    = Item String
    | ItemExposing String [String]
    | ItemEvery String

data Import = Import String (Maybe String) ImportType

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
            
                

