module Parser where

import           Data.Functor                  (($>))
import           Lexer
import           Text.Parsec
import           Text.ParserCombinators.Parsec (Parser)
import           Tree

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "nsML"

-- dummy expr parser
expr :: Parser (Expr String)
expr = Identifier <$> many alphaNum <* whiteSpace

-- | parser for ADT
typeDef :: Parser (Definition a)
typeDef = do
  reserved "type"
  ps <- many $ string "'" *> identifier
  i <- identifier
  reservedOp "="
  tys <- sepEndBy1 typeDefTerm (reservedOp "|")
  pure $ TypeDef ps i tys

typeDefTerm :: Parser ADT
typeDefTerm = do
  i <- identifier
  ty <- optionMaybe $ reserved "of" *> sepEndBy1 typeTerm (reservedOp "*")
  pure
    $ case ty of
      Nothing -> Atom i
      Just ss -> Product i ss

typeTerm :: Parser MLType
typeTerm = do
  ps <- many
    $ (Var <$> (string "'" *> identifier))  -- type var
    <|> primitiveTypes
    <|> parens funTypeTerm
  ty <- optionMaybe (Var <$> identifier <|> primitiveTypes)
  case (ps, ty) of
    ([v], Nothing)     -> pure v
    (vs, Just (Var t)) -> pure $ Custom vs t
    ([], Just pm)      -> pure pm
    _                  -> parserFail "Error: Fail to parse type"

-- | Parser for types
funTypeTerm :: Parser MLType
funTypeTerm = chainr1 typeTerm (reservedOp "->" $> MLFun)

sigDef :: Parser (FunSig String)
sigDef = do
  reserved "val"
  i <- identifier
  reservedOp ":"
  FunSig i <$> funTypeTerm

funDef :: Parser (Definition String)
funDef = do
  sig <- sigDef
  reserved "let"
  fn <- identifier
  params <- sepEndBy1 identifier whiteSpace
  reservedOp "="
  body <- expr
  let fm = funName sig
    in if fm /= fn
       then parserFail
         $ "Error: function name mismatch for " ++ fm ++ " and " ++ fn
       else pure $ FunDef sig params body

varDef :: Parser (Definition String)
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

test1 = regularParse sigDef "val map : ('a -> 'b) -> 'a list -> 'b list"

test2 = regularParse typeDef "type 'a list = nil | cons of 'a * 'a list"

test3 = regularParse funDef "val add1 : 'a -> 'a\nlet add1 x = y"

test4 = regularParse funTypeTerm "('a -> 'b) option -> 'b option"
