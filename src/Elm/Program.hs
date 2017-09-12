{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Safe              #-}

-- | Module for creating a program
module Elm.Program where

import           Protolude            hiding (join)

import           Control.Monad.Writer hiding (join)
import           Data.List            hiding (map)
import           Data.String
import           Data.String.Utils
import           Elm.Classes
import           Elm.Decleration
import           Elm.Import
import           Elm.ParseError
import           Text.PrettyPrint

-- | Program type
data Program = Program String ImportType [Import] [Dec]

instance Generate Program where
    generate (Program name exports imports declerations) = do
        exportDoc <- generate exports
        importDocs <- mapM generate imports
        decDocs <- mapM generate declerations
        return $ "module" <+> text name <+> "exposing" <+> exportDoc $+$
            vcat importDocs $+$
            vcat decDocs


-- | Convert a program to a string of code with newlines between declerations
renderProgram :: Program -> (String, GenError)
renderProgram program =
    let
       (doc, parseError) = runWriter $ generate program
       str = render doc
    in
        ((join "\n" . map addNewline . split "\n" $ str)++"\n", parseError)
    where
        addNewline line =
            if (or $ map (\s -> isInfixOf s line) [":", "type"]) then
                "\n\n" ++ line
            else
                line
