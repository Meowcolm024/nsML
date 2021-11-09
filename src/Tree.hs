module Tree where
import           Data.List                      ( intercalate )

data MLType = Var String
            | Custom [String] String
            | MLInt
            | MLString
            | MLBool
            | MLFun MLType MLType

instance Show MLType where
    show (Var a      ) = "'" ++ a
    show (Custom xs t) = unwords (("'" ++) <$> xs) ++ " " ++ t
    show MLInt         = "int"
    show MLString      = "string"
    show MLBool        = "bool"
    show (MLFun x y)   = "(" ++ show x ++ ") -> (" ++ show y ++ ")"


data ADT = Atom String
         | Product String [MLType]

instance Show ADT where
    show (Atom s     ) = s
    show (Product s t) = s ++ " of " ++ intercalate " * " (map show t)


data TypeDef = TypeDef
    { typeParams   :: [String]
    , typeName     :: String
    , constructors :: [ADT]
    }

instance Show TypeDef where
    show (TypeDef p n c) =
        "type " ++ unwords (("'" ++) <$> p) ++ " " ++ n ++ " = " ++ intercalate
            " | "
            (map show c)

data FunSig a = FunSig
    { funName :: a
    , funType :: MLType
    }
    deriving Show

data FunDef a = FunDef
    { funSig    :: FunSig a
    , funParams :: [a]
    , funBody   :: Expr a
    }
    deriving Show

data VarDef a = VarDef
    { varName :: a
    , varType :: MLType
    , varBody :: Expr a
    }
    deriving Show

class Definition a

instance Definition TypeDef
instance Definition (FunDef a)
instance Definition (VarDef a)

data Expr a = Identifier a
            -- lit
            | LitInt Int
            | LitBool Bool
            | LitString String
            | LitUnit
            -- bin op
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
            -- unary op
            | Not (Expr a)
            | Neg (Expr a)
            | Call a [Expr a]                       -- ^ fun/constr call
            | Let (a, Expr a) (Expr a)              -- ^ let binding
            | IfElse (Expr a) (Expr a) (Expr a)     -- ^ if then else
            | Match (Expr a) [MatchCase a]          -- ^ pattern matching
            | Lambda a (Expr a)                     -- ^ lambda expr
            | Bottom (Expr a)                       -- ^ error expr
            deriving Show

data MatchCase a = MatchCase (Pattern a) (Expr a)
    deriving Show

data Pattern a = WildcardPattern
               | IdPattern a
               | LiteralPattern (Expr a)
               | CustomPattern a [Pattern a]
               deriving Show
