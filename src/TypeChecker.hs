module TypeChecker where
import Syntax

doesValueMatchPrimitiveType :: Value -> PrimitiveType -> Bool
doesValueMatchPrimitiveType (BoolVal _) BoolType = True
doesValueMatchPrimitiveType (StringVal _) StringType = True
doesValueMatchPrimitiveType (NumberVal _) NumberType = True
doesValueMatchPrimitiveType (ObjectVal _) ObjectType = True
doesValueMatchPrimitiveType UndefinedVal UndefinedType = True
doesValueMatchPrimitiveType NullVal NullType = True
doesValueMatchPrimitiveType _ AnyType = True
doesValueMatchPrimitiveType _ _ = False

doesValueMatchType :: Value -> Type -> Bool
doesValueMatchType value (PrimitiveType t) = doesValueMatchPrimitiveType value t
doesValueMatchType value (UnionType types) = any (doesValueMatchPrimitiveType value) types
doesValueMatchType value (MaybeType t) = 
    doesValueMatchPrimitiveType value t 
    || doesValueMatchPrimitiveType value UndefinedType 
    || doesValueMatchPrimitiveType value NullType

expressionValid :: Expression -> Bool
expressionValid (Val val) = undefined
expressionValid (Var var) = undefined
expressionValid (Op1 uop e) = undefined
expressionValid (Op2 e1 bop e2) = undefined
expressionValid (Call es) = undefined