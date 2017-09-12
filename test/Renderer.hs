module Renderer where

import           Control.Monad.Writer
import           Elm.Classes
import           Elm.Expression
import           Elm.GenError
import           Text.PrettyPrint

renderExpr :: Expr -> String
renderExpr expr =
    let
        (doc, err) = runWriter . generate $ expr
    in
        if err == WarningList [] then
            Text.PrettyPrint.render doc
        else
            error $ "Generation Error: " ++ show err


render :: (Generate a) => a -> String
render expr =
    let
        (doc, err) = runWriter . generate $ expr
    in
        if err == WarningList [] then
             Text.PrettyPrint.render doc
        else
            error $ "Generation Error: " ++ show err
