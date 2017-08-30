{-# OPTIONS_HADDOCK prune #-}

-- | Module for creating a program
module Elm.Program where

import Elm.Import
import Elm.Decleration
import Text.PrettyPrint
import Data.String.Utils
import Data.List

-- | Program type
data Program = Program String ImportType [Import] [Dec]

genProgram :: Program -> Doc
genProgram (Program name exports imports declerations) =
    text "module" <+> text name <+> exposingDoc exports 
    $+$ (foldl ($+$) empty . map toDocI $ imports)
    $+$ (foldl ($+$) empty . map toDocD $ declerations)

-- | Convert a program to a string of code
renderProgram :: Program -> String
renderProgram program =
    let 
        str = (render . genProgram $ program) ++ "\n"
    in
        join "\n" . map addNewline . split "\n" $ str
    where
        addNewline line =
            if (or $ map (\s -> isInfixOf s line) [":", "type"]) then
                "\n" ++ line
            else
                line
