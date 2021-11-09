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
            -- fun/constr call
            | Call a [Expr a]
            | Let (a, Expr a) (Expr a)
            | IfElse (Expr a) (Expr a) (Expr a)
            | Match (Expr a) [MatchCase a]
            | Error (Expr a)
            deriving Show

data MatchCase a = MatchCase (Pattern a) (Expr a)
    deriving Show

data Pattern a = WildcardPattern
               | IdPattern a
               | LiteralPattern (Expr a)
               | CustomPattern a [Pattern a]
               deriving Show
