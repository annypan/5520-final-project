module TypeChecker where
import Syntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import State (State)
import qualified State as S
import Data.Bool (bool)
import qualified Data.Set as Set

type TypeDeclaration = Map Name Type

-- Returns the CheckResult value of a boolean
boolToCheckResult :: Bool -> CheckResult
boolToCheckResult True = Success
boolToCheckResult False = Failure

-- Checks if a value can be used as a given primitive type
doesValueMatchPrimitiveType :: Value -> PrimitiveType -> Bool
doesValueMatchPrimitiveType (BoolVal _) BoolType = True
doesValueMatchPrimitiveType (StringVal _) StringType = True
doesValueMatchPrimitiveType (NumberVal _) NumberType = True
doesValueMatchPrimitiveType (ObjectVal _) ObjectType = True
doesValueMatchPrimitiveType UndefinedVal UndefinedType = True
doesValueMatchPrimitiveType NullVal NullType = True
doesValueMatchPrimitiveType _ AnyType = True
doesValueMatchPrimitiveType _ _ = False

-- Checks if a value can be used as a given type
doesValueMatchType :: Value -> Type -> CheckResult
doesValueMatchType value (PrimitiveType t) = boolToCheckResult (doesValueMatchPrimitiveType value t)
doesValueMatchType value (UnionType types) = boolToCheckResult (any (doesValueMatchPrimitiveType value) types)
doesValueMatchType value (MaybeType t) =
    boolToCheckResult
        (doesValueMatchPrimitiveType value t
        || doesValueMatchPrimitiveType value UndefinedType
        || doesValueMatchPrimitiveType value NullType
        )

-- Checks if a type can be used when another type is expected
canBeUsedAsType :: Type -> Type -> Bool
canBeUsedAsType (PrimitiveType t1) (PrimitiveType t2) =
    t1 == t2
    || t2 == AnyType
    || t2 == BoolType && t1 /= UndefinedType && t1 /= NullType && t1 /= EmptyType && t1 /= VoidType
canBeUsedAsType (PrimitiveType t1) (UnionType ts) =
    any (canBeUsedAsType (PrimitiveType t1) . PrimitiveType) ts
canBeUsedAsType t1'@(PrimitiveType t1) t2'@(MaybeType t2) =
    canBeUsedAsType t1' t2'
    || t1 == UndefinedType
    || t1 == NullType
canBeUsedAsType (UnionType ts1) (UnionType ts2) = Set.fromList ts1 == Set.fromList ts2
canBeUsedAsType (MaybeType t1) (MaybeType t2) = canBeUsedAsType (PrimitiveType t1) (PrimitiveType t2)
canBeUsedAsType _ _ = False

-- Checks if an expression can be used as a given type
doesExpressionMatchType :: Expression -> Type -> State TypeDeclaration CheckResult
doesExpressionMatchType (Val value) t = return (doesValueMatchType value t)
doesExpressionMatchType (Var var) t =
    case var of
        Name name -> do
            store <- S.get
            case store !? name of
                Just t' -> return (boolToCheckResult (canBeUsedAsType t' t))
                Nothing -> return Unknown -- no static type info, do not raise error
        _ -> return Unknown
doesExpressionMatchType (Op1 uop e) t =
    case e of
        Val value -> undefined -- TODO: check whether the given `uop` can be applied to the given `value`
        Var (Name name) -> do
            store <- S.get
            case store !? name of
                Just t' -> undefined -- TODO: check whether the given `uop` can be applied to the given `t'`
                Nothing -> return Unknown
        _ -> return Unknown
doesExpressionMatchType (Op2 e1 bop e2) t =
    case (e1, e2) of
        (Val value1, Val value2) -> undefined -- TODO: check whether the given `bop` can be applied to the given `value1` and `value2`
        (Var (Name name1), Var (Name name2)) -> do
            store <- S.get
            case (store !? name1, store !? name2) of
                (Just t1, Just t2) -> undefined -- TODO: helper function for whether t1 and t2 can be used with bop
                _ -> return Unknown -- no static type info, do not raise error
        (Var (Name name), Val value) -> do
            store <- S.get
            case store !? name of
                Just t' -> undefined -- TODO: check whether the given `bop` can be applied to the given `t'` and `value`
                Nothing -> return Unknown -- no static type info, do not raise error
        (Val value, Var (Name name)) -> do
            store <- S.get
            case store !? name of
                Just t' -> undefined
                Nothing -> return Unknown -- no static type info, do not raise error
        _ -> return Unknown
doesExpressionMatchType (Call fn es) t = undefined

-- Checks if an expression type checks
expressionValid :: Expression -> Bool
expressionValid (Val value) = True
expressionValid (Var var) = case var of
    Name _ -> True
    Dot t _ -> undefined
    Proj t _ -> undefined
expressionValid (Op1 uop e) = undefined
expressionValid (Op2 e1 bop e2) = undefined
expressionValid (Call fn es) = undefined
