module Parser where

import           Data.Functor                  (($>))
import           Lexer
import           Text.Parsec
import           Text.ParserCombinators.Parsec (Parser)
import           Tree
import Text.Parsec.Token (natural)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "nsML"

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

-- | function application
funApp :: Parser (Expr String)
funApp = chainl1 (parens expr <|> literals <|> variable) (whiteSpace $> Call)

-- | pattern matching
matches :: Parser (Expr String)
matches = do
    reserved "match"
    e <- expr
    reserved "with"
    reservedOp "|"
    Match e <$> patterns

patterns :: Parser [MatchCase String]
patterns = sepBy1 patCase (reservedOp "|")
  where
    patCase = do
      pat <- singlePattern
      reservedOp "->"
      MatchCase pat <$> expr

singlePattern :: Parser (Pattern String)
singlePattern = wildcardPattern <|> literalPattern <|> idPattern <|> parens customPattern
  where
    wildcardPattern = reserved "_" $> WildcardPattern 
    literalPattern =  LiteralPattern <$> (literals <|> uOps literals)
    idPattern = IdPattern <$> identifier 
    customPattern = CustomPattern <$> identifier <*> many singlePattern

-- | literals
literals :: Parser (Expr a)
literals = primitiveValues

-- | error type
bottom :: Parser (Expr String)
bottom = Bottom <$> (reserved "error" *> expr)

-- | variable
variable :: Parser (Expr String)
variable = Identifier <$> identifier

-- | unary operator
uOps :: Parser (Expr String) -> Parser (Expr String)
uOps term = Not <$> (reservedOp "!" *> term) <|> Neg <$> (reservedOp "-" *> term)

-- | expr
expr :: Parser (Expr String)
expr = letIn <|> ifElse <|> matches <|> lambda <|> term0

-- | binary operators
term0 = term1 `chainl1` (reservedOp "||" $> Or)
term1 = term2 `chainl1` (reservedOp "&&" $> And)
term2 = term3 `chainl1` (reservedOp "==" $> Equals)
term3 = term4 `chainl1` (reservedOp "<" $> LessThan <|> reservedOp "<=" $> LessEqual)
term4 = term5 `chainl1` (reservedOp "+" $> Plus <|> reservedOp "-" $> Minus <|> reservedOp "++" $> Concat)
term5 = term6 `chainl1` (reservedOp "*" $> Mult <|> reservedOp "/" $> Div)
term6 = uOps term7 <|> term7
term7 = parens expr <|> literals <|> funApp <|> bottom
