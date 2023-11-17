import Data.Map

type Object = Map PrimitiveValue PrimitiveValue

data PrimitiveValue
    = BoolVal Bool -- https://flow.org/en/docs/types/literals/
    | StringVal String
    | NumberVal Double
    | NullVal
    | UndefinedVal
    | EmptyVal -- https://flow.org/en/docs/types/empty/
    | AnyVal -- https://flow.org/en/docs/types/any/
    | ObjectVal Object -- https://flow.org/en/docs/types/objects/
    deriving (Eq, Show, Ord)

data Value
    = PrimitiveVal PrimitiveValue
    | EitherVal (Either PrimitiveValue PrimitiveValue)
    | MaybeVal (Maybe PrimitiveValue) -- https://flow.org/en/docs/types/maybe/
    deriving (Eq, Show, Ord)