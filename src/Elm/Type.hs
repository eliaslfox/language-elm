{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE OverloadedStrings #-} 

-- | Ast for declaring types
module Elm.Type where

import Elm.Expression
import Text.PrettyPrint
import Data.Maybe

-- | Data type to represent types
data TypeDec
    -- | A type with type paramaters
    = Params String [TypeDec]
    -- | A function type
    | TApp [TypeDec]
    -- | A two tuple type
    | TTuple2 TypeDec TypeDec
    -- | A record type
    | TRecord (Maybe String) [(String, TypeDec)]

-- | Shortcut for declaring a type variable
tvar :: String -> TypeDec
tvar str =
    Params str []

vopTApp :: TypeDec -> Doc
vopTApp t =
    case t of
        Params str types ->
            toDocT $ Params str types
            
        TRecord main decs ->
            toDocT $ TRecord main decs

        _ ->
            parens $ toDocT t

vopParam :: TypeDec -> Doc
vopParam t =
    case t of
        Params str [] ->
            text str

        _ ->
            parens $ toDocT t
        

toDocT :: TypeDec -> Doc
toDocT t =
    case t of
        Params p decs ->
            text p <+> (hsep . map vopParam $ decs)

        TApp types ->
            hsep . punctuate (text " ->") . map vopTApp $ types
            
        TTuple2 t1 t2 ->
            lparen <> toDocT t1 <> comma <+> toDocT t2 <> rparen

        TRecord Nothing [] ->
            "{}"

        TRecord (Just main) [] ->
            text main

        TRecord main decs ->
            let
                front = fmap (\x -> text x <+> "|") main
            in
                "{" <+> Data.Maybe.fromMaybe empty front
                <+> (hsep . punctuate "," . map docDec $ decs)
                <+>  "}"
            where 
                docDec (name, dec) =
                    text name <+> ":" <+> toDocT dec
