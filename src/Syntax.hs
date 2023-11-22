module Syntax where

import Data.Map
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

type Object = Map PrimitiveValue PrimitiveValue
type Name = String
type FunctionArg = (Name, Type)

data Statement
  = Assign Var Expression -- x = e -- handles var, let, const
  | If Expression Block Block -- if e then s1 else s2 end
  | While Expression Block -- while e do s end
  | Empty -- ';'
  | For Statement Expression Expression Block -- for (s1; e1; e2) {s2}
  | Return Expression -- return e
  | FunctionDef Name [FunctionArg] Type Block -- function f(x1, ..., xn) s

data Var
  = Name Name -- x, global variable
  | Dot Expression Name -- t.x, access table using string key
  | Proj Expression Expression -- t[1], access table table using any type of key
  deriving (Eq, Show)

newtype Block = Block [Statement] -- s1 ... sn

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

data Expression
  = Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  | Call [Expression] -- function calls
  deriving (Eq, Show)

data PrimitiveValue
  = BoolVal Bool -- https://flow.org/en/docs/types/literals/
  | StringVal String
  | NumberVal Double
  | ObjectVal Object -- https://flow.org/en/docs/types/objects/
  deriving (Eq, Show)

data PrimitiveType
  = BoolType
  | StringType
  | NumberType
  | NullType
  | UndefinedType
  | EmptyType -- https://flow.org/en/docs/types/empty/
  | AnyType -- https://flow.org/en/docs/types/any/
  | ObjectType
  | VoidType -- only used for function return types
  deriving (Eq, Show)

data Type
  = PrimitiveType PrimitiveType
  | EitherType (Either PrimitiveType PrimitiveType)
  | MaybeType (Maybe PrimitiveType)

data Value
  = PrimitiveVal PrimitiveValue
  | EitherVal (Either PrimitiveValue PrimitiveValue)
  | MaybeVal (Maybe PrimitiveValue) -- https://flow.org/en/docs/types/maybe/
  deriving (Eq, Show)

data Uop
  = Neg
  | Not
  | TypeOf -- https://www.digitalocean.com/community/tutorials/javascript-unary-operators-simple-and-useful#typeof
  deriving (Eq, Show, Enum, Bounded)

data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Eq
  | Neq
  | Gt
  | Ge
  | Lt
  | Le
  | Concat
  | In -- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
  deriving (Eq, Show, Enum, Bounded)

class PP a where
  pp :: a -> Doc

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: (PP a) => a -> String
pretty = PP.render . pp

-- TODO: Define a pretty printer for each type