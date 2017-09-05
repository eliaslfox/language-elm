{-# OPTIONS_HADDOCK prune #-}

-- | Top level declerations
module Elm.Decleration where

import           Elm.Expression
import           Elm.Type
import           Text.PrettyPrint

-- | Used to declare functions, variables, and types
data Dec
    -- | Declare a function
    = Dec String TypeDec [Expr] Expr
    -- | Declare a type
    | DecType String [String] [(String, [TypeDec])]
    -- | Declare a type alias
    | DecTypeAlias String [String] TypeDec

toDoc = undefined
toDocT = undefined
vopParam = undefined

toDocD :: Dec -> Doc
toDocD dec =
    case dec of
        Dec str typeDec args body ->
               text str <+> text ":" <+> toDocT typeDec $+$
               hang (text str <+> (hsep . map toDoc $ args) <+> text "=") 4 (toDoc body)

        DecTypeAlias str typeParams t->
            text "type alias" <+> text str <+> (hsep . map text $ typeParams) <+> text "=" <+> toDocT t

        DecType str typeParams types ->
            text "type" <+> text str <+> (hsep . map text $ typeParams) <+> text "=" <+>
                (hsep . punctuate (text " |") . map toDec $ types)

            where
                toDec (str, t) =
                    text str <+> (hsep . map vopParam $ t)
