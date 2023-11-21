module Syntax where

import Data.Map
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP

type Object = Map PrimitiveValue PrimitiveValue

data Expression
  = Val Value -- literal values
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  deriving (Eq, Show)

data PrimitiveValue
  = BoolVal Bool -- https://flow.org/en/docs/types/literals/
  | StringVal String
  | NumberVal Double
  | NullVal
  | UndefinedVal
  | EmptyVal -- https://flow.org/en/docs/types/empty/
  | AnyVal -- https://flow.org/en/docs/types/any/
  | ObjectVal Object -- https://flow.org/en/docs/types/objects/
  deriving (Eq, Show)

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