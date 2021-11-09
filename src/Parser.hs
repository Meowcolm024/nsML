module Parser where

import           Data.Functor                   ( ($>) )
import           Lexer
import           Text.Parsec
import qualified Text.Parsec.Token             as P
import           Text.ParserCombinators.Parsec  ( Parser )
import           Tree


regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "nsML"

-- dummy expr parser
expr :: Parser (Expr String)
expr = Identifier <$> many alphaNum <* whiteSpace

-- | parser for ADT
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
    case (ps, ty) of
        ([v], Nothing) -> pure $ Var v
        (vs , Just t ) -> pure $ Custom vs t
        _              -> parserFail $ "Too many type-args " ++ show ps

-- | Parser for types
funTypeTerm :: Parser MLType
funTypeTerm = chainr1 typeTerm (reservedOp "->" $> MLFun)

sigDef :: Parser (FunSig String)
sigDef = do
    reserved "val"
    i <- identifier
    reservedOp ":"
    FunSig i <$> funTypeTerm

funDef :: Parser (FunDef String)
funDef = do
    sig <- sigDef
    reserved "let"
    fn     <- identifier
    params <- sepEndBy1 identifier whiteSpace
    reservedOp "="
    body <- expr
    let fm = funName sig in if fm /= fn
        then parserFail $ "Error: function name mismatch for " ++ fm ++ " and " ++ fn
        else pure $ FunDef sig params body

varDef :: Parser (VarDef String)
varDef = do
    reserved "let"
    i <- identifier
    reservedOp ":"
    ty <- funTypeTerm
    reservedOp "="
    VarDef i ty <$> expr


-- | if then else
ifElse :: Parser (Expr String)
ifElse = do
    reserved "if"
    p <- expr
    reserved "then"
    x <- expr
    reserved "else"
    IfElse p x <$> expr

-- | let binding
letIn :: Parser (Expr String)
letIn = do
    reserved "let"
    i <- identifier
    reservedOp "="
    v <- expr
    reserved "in"
    Let (i, v) <$> expr

-- | lambda expr
lambda :: Parser (Expr String)
lambda = do
    reserved "fun"
    i <- identifier
    reserved "->"
    Lambda i <$> expr

-- | error type
bottom :: Parser (Expr String)
bottom = Bottom <$> (reserved "error" *> expr)

test = regularParse sigDef "val map : ('a -> 'b) -> 'a list -> 'b list"
test2 = regularParse typeDef "type 'a list = nil | cons of 'a * 'a list"
test3 = regularParse funDef "val add1 : 'a -> 'a\nlet add1 x = y"
