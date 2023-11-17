data PrimitiveValue
    = BoolVal Bool -- https://flow.org/en/docs/types/literals/
    | StringVal String
    | NumberVal Double
    | NullVal
    | UndefinedVal
    | EmptyVal -- https://flow.org/en/docs/types/empty/
    | AnyVal -- https://flow.org/en/docs/types/any/
    
data Value
    = PrimitiveVal PrimitiveValue
    | EitherVal (Either PrimitiveValue PrimitiveValue)
    | MaybeVal (Maybe PrimitiveValue) -- https://flow.org/en/docs/types/maybe/