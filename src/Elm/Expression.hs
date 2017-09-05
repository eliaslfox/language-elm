{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy       #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Used to declare expressions
module Elm.Expression
    ( Expr(..)
    ) where

import           Control.Monad        (mapM, when)
import           Control.Monad.Writer (tell)
import           Data.String          (IsString (..))
import           Elm.Classes          (Generate (..))
import           Elm.ParseError       (GenError (..))
import           GHC.Exts             (IsList (..))
import           Text.PrettyPrint     hiding (Str)

-- | The expression type
data Expr {-
      Constants
    -}
    -- | A boolean literal
    = Bool Bool
    -- | A string literal
    | Str String
    -- | An integer literal
    | Int Int
    -- | A float literal
    | Float Float
    -- | An underscore variable placeholder
    | Under
    {-
      Inline
    -}
    -- | A variable
    | Var String
    -- | Function application, the tail is applied to the head
    | App [Expr]
    -- | A list of expressions
    | List [Expr]
    -- | Apply an inline operator to two expressions
    | Op String
         Expr
         Expr
    -- | A tuple of expressions
    | Tuple [Expr]
    -- | A record, the first paramater is an optional record to update from
    | Record (Maybe Expr)
             [(String, Expr)]
    {-
      Multi Line
    -}
    -- | A let expression
    | Let Expr
          [(Expr, Expr)]
    -- | A case expression
    | Case Expr
           [(Expr, Expr)]
    {-
      Util
    -}
    -- | Wrap an expression in parens, should be mostly automatic
    | Parens Expr

-- | Allows creating variables with overloaded strings
instance IsString Expr where
    fromString = Var

-- | Allows creating lists with overloaded lists
instance IsList Expr where
    type Item Expr = Expr
    fromList = List
    toList = error "toList is not defined on expressions"

instance Generate Expr where
    generate expr =
        case expr of
            Var str -> do
                when (str == "") $
                    tell $ Error "An empty string is not a valid variable name"
                return $ text str
            App []
            -- I don't think this has a valid meaning
             -> do
                tell $ Error "Invalid syntax, trying to apply nothing"
                return $ text ""
            App [expr'] -> generate expr'
            App exprs
            -- If only I could understand my own code :(
             -> do
                docs <- mapM vop exprs
                return . hsep $ docs
            Tuple items -> do
                when (length items > 9) $
                    tell $ Error "Length of tuple is too long"
                when (length items > 7) $
                    tell $
                    WarningList
                        [ "Tuples of length longer than seven are not comparable"
                        ]
                docs <- mapM generate items
                return . parens . hsep . punctuate "," $ docs
            Str str -> return . doubleQuotes . text $ str
            Op op expr1 expr2 -> do
                doc1 <- vop expr1
                doc2 <- vop expr2
                return $ doc1 <+> text op <+> doc2
            Case _ [] -> do
                tell $ Error "Unable to create case expression with 0 cases"
                return ""
            Case value options -> do
                docValue <- generate value
                optionsList <- genCaseList options
                return $ "case" <+> docValue <+> "of" $+$ nest 4 optionsList
            List items -> do
                docs <- mapM generate items
                return . brackets . hsep . punctuate "," $ docs
            Let _ [] -> do
                tell $ Error "Unable to create let expression with 0 bindings"
                return ""
            Let value bindings -> do
                bindingsList <- genLetList bindings
                valueDoc <- generate value
                return $
                    "let" $+$ nest 4 bindingsList $+$ "in" $+$ nest 4 valueDoc
            Int val -> do
                when (val > 9007199254740991) $
                -- I would love for someone, somewhere, to get this warning
                    tell $
                    WarningList
                        [ "The number " ++
                          show val ++
                          " is larger than the largest safe number in js"
                        ]
                return . int $ val
            Float val -> do
                when (val > 9007199254740991) $
                    tell $
                    WarningList
                        [ "The number " ++
                          show val ++
                          " is larger that the largest safe number in js"
                        ]
                return . float $ val
            Under -> return . char $ '_'
            Bool bool ->
                if bool
                    then return . text $ "True"
                    else return . text $ "False"
            Record Nothing [] -> return "{}"
            Record (Just (Var str)) []
            -- tbh, what would you even be trying to do?
             -> do
                tell $
                    WarningList
                        [ "Trying to update record " ++
                          str ++ " with no changed fields"
                        ]
                return . text $ str
            Record (Just (Var str)) updates -> do
                list <- genRecordList updates
                return $ lbrace <+> text str <+> "|" <+> list <+> rbrace
            Record (Just _) _
            -- This seems to be how it is
             -> do
                tell $
                    Error
                        "You are unable to update a record with a non constant"
                return ""
            Record Nothing updates -> do
                list <- genRecordList updates
                return $ lbrace <+> list <+> rbrace
            Parens expr' -> do
                doc <- generate expr'
                return . parens $ doc
            -- Generates the list of key value pairs in a record
      where
        genRecordList updates = do
            let (keys, values) = unzip updates
            let docKeys = map text keys
            docValues <- mapM generate values
            return . hsep . punctuate "," . map (\(a, b) -> a <+> "=" <+> b) $
                zip docKeys docValues
            -- Generates the list of declerations in a let expression
        genLetList bindings = do
            let (keys, values) = unzip bindings
            docKeys <- mapM generate keys
            docValues <- mapM generate values
            return . vcat . map (\(a, b) -> a <+> "=" <+> b) $
                zip docKeys docValues
            -- Generates the list of cases in a case statement
        genCaseList options = do
            let (keys, values) = unzip options
            docKeys <- mapM generate keys
            docValues <- sequence . map generate $ values
            return . vcat . map (\(a, b) -> a <+> "->" $+$ nest 4 b) $
                zip docKeys docValues
            -- takes an expression and wraps it in parens
            -- if required for nesting it in another expression
        vop expr' =
            case expr' of
                Var _ -> generate expr'
                Tuple _ -> generate expr'
                List _ -> generate expr'
                Int _ -> generate expr'
                Float _ -> generate expr'
                Under -> generate expr'
                Str _ -> generate expr'
                Record _ _ -> generate expr'
                _ -> do
                    doc <- generate expr'
                    return . parens $ doc
