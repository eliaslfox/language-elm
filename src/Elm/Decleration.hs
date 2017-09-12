{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}

-- | Top level declerations
module Elm.Decleration where

import           Elm.Classes
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

instance Generate Dec where
    generate dec =
        case dec of
             Dec name type_ params value -> do
                typeDoc <- generate type_
                paramDocs <- mapM generate params
                valueDoc <- generate value
                return $ text name <+> ":" <+> typeDoc $+$ text name <+> hsep paramDocs <+> "=" $+$ nest 4 valueDoc

             DecType name params instances -> do
                let (keys, values) = unzip instances
                let keyDocs = map text keys
                valueDocs <- mapM (mapM generate) values
                let instanceDocs = map (\(key, values') -> key <+> hsep values') $ zip keyDocs valueDocs
                let paramDocs = map text params
                return $ "type" <+> text name <+> hsep paramDocs $+$
                    (nest 4 $  "=" <+> head instanceDocs $+$ (vcat . map ((<+>)"|") . tail $ instanceDocs))

             DecTypeAlias name params type_ -> do
                typeDoc <- generate type_
                let paramDocs = map text params
                return $ "type alias" <+> text name <+> hsep paramDocs <+> "=" $+$ nest 4 typeDoc
