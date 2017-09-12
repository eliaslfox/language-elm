{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Ast for declaring types
module Elm.Type where

import           Control.Monad.Writer (tell)
import           Data.List            (intersperse)
import           Data.Maybe
import           Data.String
import           Elm.Classes
import           Elm.Expression
import           Elm.ParseError
import           Text.PrettyPrint

-- | Data type to represent types
data TypeDec
    -- | A type with type paramaters
    = Params String [TypeDec]
    -- | A function type
    | TApp [TypeDec]
    -- | A tuple type
    | TTuple [TypeDec]
    -- | A record type
    | TRecord (Maybe String) [(String, TypeDec)]

instance IsString TypeDec where
    fromString x = Params x []

instance Generate TypeDec where
    generate typeDec =
        case typeDec of
             Params type_ params -> do
                 docParams <- mapM vopParam params
                 return $ text type_ <+> hsep docParams

             TApp decs -> do
                 docDecs <- mapM vopTApp decs
                 return . hsep . intersperse "->" $ docDecs

             TTuple [] -> do
                return "()"

             TTuple [item] -> do
                 tell $ WarningList ["Attempt to create a one item tuple"]
                 parens <$> generate item
             TTuple items -> do
                 docItems <- mapM generate items
                 return . parens . hsep . punctuate "," $ docItems

             TRecord Nothing [] -> do
                 tell $ Error "Unable to create a record type with no base and no constraints"
                 return ""

             TRecord (Just str) [] -> do
                 tell $ WarningList ["You are creating a record type from " ++ str ++ " with no constraints"]
                 return . text $ str

             TRecord Nothing constraints -> do
                 cDoc <- generateTRecordList constraints
                 return $ lbrace <+> cDoc <+> rbrace

             TRecord (Just str) constraints -> do
                 cDoc <- generateTRecordList constraints
                 return $ lbrace <+> text str <+> text "|" <+> cDoc <+> rbrace

        where
            generateTRecordList constraints = do
                let (keys, values) = unzip constraints
                let docKeys = map text keys
                docValues <- mapM generate values
                let docList = zip docKeys docValues
                return . hsep . punctuate "," . map (\(a, b) -> a <+> ":" <+> b) $ docList

            vopParam type_ =
                case type_ of
                     Params str [] ->
                         return . text $ str

                     _ ->
                         parens <$> generate type_

            vopTApp type_ =
                case type_ of
                     TApp _ ->
                         parens <$> generate type_

                     _ ->
                         generate type_
