import Data.Map

type Object = Map Expression Value

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
