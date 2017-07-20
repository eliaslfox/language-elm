module Type where

import Expression
import Text.PrettyPrint

data TypeDec
    = Params String [TypeDec]
    | TApp [TypeDec]
    | TTuple2 TypeDec TypeDec

tvar :: String -> TypeDec
tvar str =
    Params str []

vopTApp :: TypeDec -> Doc
vopTApp t =
    case t of
        Params str types ->
            toDocT $ Params str types
            
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
