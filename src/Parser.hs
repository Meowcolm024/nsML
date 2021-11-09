module Parser where

import           Data.Functor                   ( ($>) )
import           Lexer
import           Text.Parsec
import qualified Text.Parsec.Token             as P
import           Text.ParserCombinators.Parsec  ( Parser )
import           Tree


regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "nsML"


typeDef :: Parser TypeDef
typeDef = do
    reserved "type"
    ps <- many $ string "'" *> identifier
    i  <- identifier
    reservedOp "="
    tys <- sepEndBy1 typeDefTerm (reservedOp "|")
    pure $ TypeDef ps i tys

typeDefTerm :: Parser ADT
typeDefTerm = do
    i  <- identifier
    ty <- optionMaybe $ reserved "of" *> sepEndBy1 typeTerm (reservedOp "*")
    pure $ case ty of
        Nothing -> Atom i
        Just ss -> Product i ss

typeTerm :: Parser MLType
typeTerm = parens funTypeTerm <|> do
    ps <- many $ string "'" *> identifier
    ty <- optionMaybe (identifier <|> primitiveTypes)
    pure $ case (ps, ty) of
        ([v], Nothing) -> Var v
        (vs , Just t ) -> Custom vs t
        _              -> error $ show ps ++ "\n" ++ show ty

funTypeTerm :: Parser MLType
funTypeTerm = chainr1 typeTerm (reservedOp "->" $> MLFun)

funDef :: Parser (String, [String], String)
funDef = do
    reserved "fun"
    fn     <- identifier
    params <- sepEndBy1 identifier whiteSpace
    reservedOp "="
    body <- many alphaNum <* whiteSpace 
    pure (fn, params, body)

sigDef :: Parser (String, String)
sigDef = do
    reserved "val"
    i <- identifier
    reservedOp ":"
    tys <- funTypeTerm
    pure (i, show tys)

ifElse :: Parser (String, String, String)
ifElse = do
    reserved "if"
    p <- many alphaNum <* whiteSpace
    reserved "then"
    x <- many alphaNum <* whiteSpace
    reserved "else"
    y <- many alphaNum <* whiteSpace
    pure (p, x, y)

letIn :: Parser (String, String)
letIn = do
    reserved "let"
    d <- many alphaNum <* whiteSpace
    reserved "in"
    e <- many alphaNum <* whiteSpace
    pure (d, e)



test = regularParse sigDef "val map : ('a -> 'b) -> 'a list -> 'b list"
test2 = regularParse typeDef "type 'a list = nil | cons of 'a * 'a list"
