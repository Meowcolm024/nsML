{-# LANGUAGE DeriveFunctor #-}
module Types where

import           Data.List                      ( intercalate )

-- | the program
newtype Program a = Program {definitions :: [Definition a]} deriving (Show, Functor)

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
      { typeParams :: [a],
        typeName :: a,
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
    deriving (Functor)

instance Show a => Show (Definition a) where
    show (TypeDef p n c) =
        "type "
            ++ unwords (("'" ++) . show <$> p)
            ++ " "
            ++ show n
            ++ " = "
            ++ intercalate " | " (map show c)
    show (FunDef s _ b) =
        show s ++ "\nlet " ++ show (funName s) ++ " =\n" ++ show b
    show (VarDef n t b) =
        "let " ++ show n ++ " : " ++ show t ++ " =\n" ++ show b

data FunSig a = FunSig
    { funName :: a
    , funType :: MLType
    }
    deriving Functor

instance Show a => Show (FunSig a) where
    show (FunSig n t) = "val " ++ show n ++ " : " ++ show t


-- | Expressions
data Expr a
    = Identifier a
    | LitInt Integer
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
        Call (Expr a) (Expr a)
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
    deriving (Show, Functor)

data MatchCase a = MatchCase (Pattern a) (Expr a)
    deriving (Show, Functor)

data Pattern a
    = WildcardPattern
    | IdPattern a
    | LiteralPattern (Expr a)
    | CustomPattern a [Pattern a]
    deriving (Show, Functor)

-- | identifier after name analysis
data Idx = Idx
    { idName :: String      -- ^ string of the original name
    , idx    :: Int         -- ^ unique id
    }
    deriving Show
