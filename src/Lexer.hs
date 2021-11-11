module Lexer where

import           Data.Functor                   ( ($>) )
import           Text.Parsec
import qualified Text.Parsec.Token             as P
import           Text.ParserCombinators.Parsec  ( Parser )
import           Types                          ( Expr(..)
                                                , MLType(..)
                                                )

langDef :: P.LanguageDef a
langDef = P.LanguageDef
    { P.commentStart    = "(*"
    , P.commentEnd      = "*)"
    , P.commentLine     = "//"      -- it is the case in FSharp
    , P.nestedComments  = False
    , P.identStart      = letter
    , P.identLetter     = alphaNum
    , P.opStart         = oneOf "+-*/=:<>|&!"
    , P.opLetter        = oneOf "+-*/=:<>|&!"
    , P.reservedNames   = [ "type"
                          , "of"
                          , "let"
                          , "in"
                          , "fun"
                          , "val"
                          , "match"
                          , "with"
                          , "if"
                          , "then"
                          , "else"
                          , "true"
                          , "false"
                          , "bool"
                          , "int"
                          , "string"
                          , "unit"
                          , "error"
                          , "_"
                          ]
    , P.reservedOpNames = [ ":"     -- type
                          , "->"    -- function arroe
                          , "."     -- ?
                          , "++"    -- string concat
                          , "|>"    -- pipe
                          , "||"    -- or
                          , "&&"    -- and
                          , "+"     -- add
                          , "-"     -- minus / neg
                          , "*"     -- mult
                          , "/"     -- div
                          , "=="    -- equal
                          , "="     -- binding
                          , "<"     -- lt
                          , "<="    -- le
                          ]
    , P.caseSensitive   = True
    }

lexer = P.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser String
identifier = P.identifier lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

primitiveTypes :: Parser MLType
primitiveTypes =
    reserved "int"
        $>  MLInt
        <|> reserved "bool"
        $>  MLBool
        <|> reserved "string"
        $>  MLString
        <|> reserved "unit"
        $>  MLUnit


primitiveValues :: Parser (Expr a)
primitiveValues =
    reserved "true"
        $>  LitBool True
        <|> reserved "false"
        $>  LitBool False
        <|> reserved "unit"
        $>  LitUnit
        <|> LitInt
        <$> P.natural lexer
        <|> LitString
        <$> P.stringLiteral lexer
