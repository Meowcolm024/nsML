module Lexer where

import           Data.Functor                   ( ($>) )
import           Text.Parsec
import qualified Text.Parsec.Token             as P
import           Text.ParserCombinators.Parsec  ( Parser )

langDef :: P.LanguageDef a
langDef = P.LanguageDef
    { P.commentStart    = "(*"
    , P.commentEnd      = "*)"
    , P.commentLine     = ";;"      -- change later
    , P.nestedComments  = False
    , P.identStart      = letter
    , P.identLetter     = alphaNum
    , P.opStart         = oneOf "+-*/=:<>()[]'\"|&!"
    , P.opLetter        = oneOf "+-*/=:<>()[]'\"|&!"
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

primitiveTypes :: Parser String
primitiveTypes =
    choice $ map (\x -> reserved x $> x) ["int", "string", "bool", "unit"]

primitiveValues :: Parser String
primitiveValues =
    choice $ map (\x -> reserved x $> x) ["true", "false", "unit"]
