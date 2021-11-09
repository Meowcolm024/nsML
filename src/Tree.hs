{-# LANGUAGE Rank2Types #-}

module Tree where

import           Data.List                      ( intercalate )

-- | the program
newtype Program a = Program {definitions :: [Definition a]} deriving (Show)

-- | Types
data MLType
  = Var String              -- ^ type variable
  | Custom [MLType] String  -- ^ adt
  | MLInt
  | MLString
  | MLBool
  | MLUnit
  | MLFun MLType MLType

instance Show MLType where
    show (Var a      ) = "'" ++ a
    show (Custom xs t) = "(" ++ (xs >>= show) ++ " " ++ t ++ ")"
    show MLInt         = "int"
    show MLString      = "string"
    show MLBool        = "bool"
    show MLUnit        = "unit"
    show (MLFun x y)   = "(" ++ show x ++ ") -> (" ++ show y ++ ")"

data ADT = Atom String              -- ^ atomic type
         | Product String [MLType]  -- ^ type constructor

instance Show ADT where
    show (Atom s     ) = s
    show (Product s t) = s ++ " of " ++ intercalate " * " (map show t)

-- | Definitions
data Definition a
  = TypeDef
      { typeParams :: [String],
        typeName :: String,
        constructors :: [ADT]
      }
  | FunDef
      { funSig :: FunSig a,
        funParams :: [a],
        funBody :: Expr a
      }
  | VarDef
      { varName :: a,
        varType :: MLType,
        varBody :: Expr a
      }

instance Show a => Show (Definition a) where
    show (TypeDef p n c) =
        "type " ++ unwords (("'" ++) <$> p) ++ " " ++ n ++ " = " ++ intercalate
            " | "
            (map show c)
    show (FunDef s _ _) = show s
    show (VarDef n t _) = "let " ++ show n ++ " : " ++ show t

data FunSig a = FunSig
    { funName :: a
    , funType :: MLType
    }

instance Show a => Show (FunSig a) where
    show (FunSig n t) = "val " ++ show n ++ " : " ++ show t

-- | Expressions
data Expr a
    = Identifier a
    | LitInt Int
    | LitBool Bool
    | LitString String
    | LitUnit
    | Plus (Expr a) (Expr a)
    | Minus (Expr a) (Expr a)
    | Mult (Expr a) (Expr a)
    | Div (Expr a) (Expr a)
    | LessThan (Expr a) (Expr a)
    | LessEqual (Expr a) (Expr a)
    | And (Expr a) (Expr a)
    | Or (Expr a) (Expr a)
    | Equals (Expr a) (Expr a)
    | Concat (Expr a) (Expr a)
    | Pipe (Expr a) (Expr a)
    | Not (Expr a)
    | Neg (Expr a)
    | -- | fun/constr call, curried
        Call a (Expr a)
    | -- | let binding
        Let (a, Expr a) (Expr a)
    | -- | if then else
        IfElse (Expr a) (Expr a) (Expr a)
    | -- | pattern matching
        Match (Expr a) [MatchCase a]
    | -- | lambda expr
        Lambda a (Expr a)
    | -- | error expr
        Bottom (Expr a)
    deriving (Show)

data MatchCase a = MatchCase (Pattern a) (Expr a)
    deriving Show

data Pattern a
    = WildcardPattern
    | IdPattern a
    | LiteralPattern (Expr a)
    | CustomPattern a [Pattern a]
    deriving (Show)
