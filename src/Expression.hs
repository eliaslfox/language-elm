module Expression where

import Text.PrettyPrint hiding (Str)
import Data.Maybe

data Expr
    = App String [Expr]
    | Case Expr [(Expr, Expr)]
    | Let Expr [(Expr, Expr)] 
    | List [Expr]
    | Tuple2 Expr Expr
    | Tuple3 Expr Expr Expr
    | Op String Expr Expr
    | Parens Expr
    | Str String
    | Int Int
    | Under
    | BoolTrue
    | BoolFalse
    | Record (Maybe Expr) [(String, Expr)]

var :: String -> Expr
var str = App str []

-- Takes an expression
-- if its a single variable or tuple then id
-- else wrap it in parens
vop :: Expr -> Doc
vop expr =
    case expr of
        App str [] ->
            text str

        Tuple2 exp1 exp2 ->
            toDoc $ Tuple2 exp1 exp2

        Tuple3 expr1 expr2 expr3 ->
            toDoc $ Tuple3 expr1 expr2 expr3

        Str str ->
            doubleQuotes $ text str

        Record a b ->
            toDoc $ Record a b

        other ->
            parens $ toDoc other

toDoc :: Expr -> Doc
toDoc expr =
    case expr of
        App str exprs ->
            text str <+> (hsep . map vop $ exprs)

        Tuple2 expr1 expr2 ->
            parens $ toDoc expr1 <> comma <+> toDoc expr2

        Tuple3 expr1 expr2 expr3 ->
            parens $ toDoc expr1 <> comma <+> toDoc expr2 <> comma <+> toDoc expr3

        Str str ->
            doubleQuotes . text $ str

        Op op expr1 expr2 ->
           vop expr1 <+> text op <+> vop expr2 

        Case expr exprs ->
            hang (text "case" <+> vop expr <+> text "of") 4 (vcat . map caseToDoc $ exprs)

            where
                caseToDoc (expr1, expr2) =
                    toDoc expr1 <+> text "->" $$ (nest 4 $ toDoc expr2)


        List exprs ->
            char '[' <> (hsep . punctuate (text ",") . map toDoc $ exprs) <> char ']'

        Let expr exprs ->
            text "let" $+$ (nest 4 . vcat . map letToDoc $ exprs) $+$ text "in" $+$ (nest 4 $ toDoc expr)
              
            where
                letToDoc (expr1, expr2) =
                    toDoc expr1 <+> char '=' <+> toDoc expr2

        Int i ->
            int i

        Under ->
            char '_'

        BoolTrue ->
            text "True"

        BoolFalse ->
            text "False"

        Record Nothing [] ->
            text "{}"

        Record (Just main) [] ->
            toDoc main

        Record main parts ->
            let
                front = fmap (\x -> toDoc x <+> char '|') main
            in
               char '{' <+> (Data.Maybe.fromMaybe empty front)
               <+> nest 4 (hsep . punctuate (char ',') . map docPart $ parts)
               <+> char '}'
            where
                docPart (name, value) =
                    text name <+> char '=' <+> toDoc value
