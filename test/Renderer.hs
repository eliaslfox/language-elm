module Renderer where

import Elm.Expression
import Elm.ParseError
import Elm.Classes
import Text.PrettyPrint
import Control.Monad.Writer

renderExpr :: Expr -> String
renderExpr expr = 
    let
        (doc, err) = runWriter . generate $ expr
    in
        if err == WarningList [] then
            render doc 
        else
            error $ "Generation Error: " ++ show err 
