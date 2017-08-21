module Elm.Decleration where

import Elm.Type
import Elm.Expression
import Text.PrettyPrint

data Dec
    = Dec String TypeDec [Expr] Expr
    | DecType String [String] [(String, [TypeDec])]
    | DecTypeAlias String [String] TypeDec

toDocD :: Dec -> Doc
toDocD dec =
    case dec of
        Dec str typeDec args body ->
               text str <+> text "::" <+> toDocT typeDec $+$
               hang (text str <+> (hsep . map toDoc $ args) <+> text "=") 4 (toDoc body)

        DecTypeAlias str typeParams t->
            text "type alias" <+> text str <+> (hsep . map text $ typeParams) <+> text "=" <+> toDocT t

        DecType str typeParams types ->
            text "type" <+> text str <+> (hsep . map text $ typeParams) <+> text "=" <+>
                (hsep . punctuate (text " |") . map toDec $ types)

            where
                toDec (str, t) =
                    text str <+> (hsep . map vopParam $ t)
