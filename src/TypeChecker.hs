module TypeChecker where
import Syntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import State (State)
import qualified State as S
import qualified Data.Set as Set

type TypeDeclaration = Map Name Type

data CheckResult
  = Success
  | Failure
  | Unknown
  deriving (Eq, Show, Ord)

-- Returns the CheckResult value of a boolean
boolToCheckResult :: Bool -> CheckResult
boolToCheckResult True = Success
boolToCheckResult False = Failure

-- Checks if a value can be used as a given primitive type
doesValueMatchPrimitiveType :: Value -> PrimitiveType -> Bool
doesValueMatchPrimitiveType UndefinedVal BoolType = False
doesValueMatchPrimitiveType NullVal BoolType = False
doesValueMatchPrimitiveType _ BoolType = True -- values other than `undefined` and `null` can be used as BoolType
doesValueMatchPrimitiveType (StringVal _) StringType = True
doesValueMatchPrimitiveType (NumberVal _) NumberType = True
doesValueMatchPrimitiveType (ObjectVal _) ObjectType = True
doesValueMatchPrimitiveType UndefinedVal UndefinedType = True
doesValueMatchPrimitiveType NullVal NullType = True
doesValueMatchPrimitiveType _ AnyType = True
doesValueMatchPrimitiveType _ _ = False

-- Checks if a value can be used as a given type
doesValueMatchType :: Value -> Type -> CheckResult
doesValueMatchType value (PrimitiveType t) =
    boolToCheckResult (doesValueMatchPrimitiveType value t)
doesValueMatchType value (UnionType types) =
    boolToCheckResult (any (doesValueMatchPrimitiveType value) types)
doesValueMatchType value (MaybeType t) =
    boolToCheckResult
        (doesValueMatchPrimitiveType value t
        || doesValueMatchPrimitiveType value UndefinedType
        || doesValueMatchPrimitiveType value NullType
        )

-- Checks if a type can be used when another type is expected
canBeUsedAsType :: Type -> Type -> Bool
canBeUsedAsType _ (PrimitiveType AnyType) = True
canBeUsedAsType (PrimitiveType t1) (PrimitiveType t2) =
    t1 == t2
    || t2 == BoolType && t1 /= UndefinedType && t1 /= NullType
       && t1 /= EmptyType && t1 /= VoidType
canBeUsedAsType (PrimitiveType t1) (UnionType ts) =
    any (canBeUsedAsType (PrimitiveType t1) . PrimitiveType) ts
canBeUsedAsType t1'@(PrimitiveType t1) t2'@(MaybeType t2) =
    t1 == t2
    || t1 == UndefinedType
    || t1 == NullType
canBeUsedAsType (UnionType ts1) (UnionType ts2) = Set.fromList ts1 `Set.isSubsetOf` Set.fromList ts2
canBeUsedAsType (MaybeType t1) (MaybeType t2) = canBeUsedAsType (PrimitiveType t1) (PrimitiveType t2)
canBeUsedAsType _ _ = False

-- Generates all super types of a given type
genSuperTypes :: Type -> [Type]
genSuperTypes (PrimitiveType t) =
    let ret = [AnyType, t] ++ ([BoolType | not (t == NullType || t == UndefinedType || t == EmptyType || t == VoidType || t == BoolType)]) in
            map PrimitiveType ret ++ map MaybeType ret ++ map (UnionType . return) ret
genSuperTypes ts = [ts]

-- Gets the primitive type of a value
getType :: Value -> PrimitiveType
getType (BoolVal _) = BoolType
getType (StringVal _) = StringType
getType (NumberVal _) = NumberType
getType (ObjectVal _) = ObjectType
getType UndefinedVal = UndefinedType
getType NullVal = NullType

-- Checks if a unary operator can be used with a given type
doesUopMatchType :: Uop -> Type -> Bool
doesUopMatchType Neg (PrimitiveType t) = case t of
    BoolType -> True
    NumberType -> True
    _ -> False
doesUopMatchType Neg (UnionType ts) = all (doesUopMatchType Neg . PrimitiveType) ts
doesUopMatchType Neg (MaybeType t) = doesUopMatchType Neg (PrimitiveType t)
doesUopMatchType Not (PrimitiveType t) = canBeUsedAsType (PrimitiveType t) (PrimitiveType BoolType)
doesUopMatchType Not (UnionType ts) = all (doesUopMatchType Not . PrimitiveType) ts
doesUopMatchType Not (MaybeType t) = doesUopMatchType Not (PrimitiveType t)
doesUopMatchType TypeOf _ = True

-- Checks if a unary operator can be used with a given value
doesUopMatchValue :: Uop -> Value -> Bool
doesUopMatchValue uop val = any (doesUopMatchType uop) (genSuperTypes (PrimitiveType (getType val)))

-- Checks if an arithmetic operator can be used with given types
doesArithMatchType :: Bop -> Type -> Type -> Bool
doesArithMatchType arith (PrimitiveType t1) (PrimitiveType t2) = case (t1, t2) of
    (NumberType, NumberType) -> True
    _ -> False
doesArithMatchType arith (UnionType ts1) (PrimitiveType t2) =
    all (doesArithMatchType arith (PrimitiveType t2) . PrimitiveType) ts1
doesArithMatchType arith (PrimitiveType t1) (UnionType ts2) =
    all (doesBopMatchType arith (PrimitiveType t1) . PrimitiveType) ts2
doesArithMatchType arith (UnionType ts1) (UnionType ts2) =
    all (\t1 -> all (doesBopMatchType arith (PrimitiveType t1) . PrimitiveType) ts2) ts1
doesArithMatchType _ _ _ = False

doesCompMatchType :: Bop -> Type -> Type -> Bool
doesCompMatchType bop (PrimitiveType t1) (PrimitiveType t2) = t1 == t2
doesCompMatchType _ _ _ = False

-- Checks if a binary operator can be used with given types
doesBopMatchType :: Bop -> Type -> Type -> Bool
doesBopMatchType bop t1 t2 = case bop of
    Plus -> doesArithMatchType bop t1 t2
    Minus -> doesArithMatchType bop t1 t2
    Times -> doesArithMatchType bop t1 t2
    Divide -> doesArithMatchType bop t1 t2
    Modulo -> doesArithMatchType bop t1 t2
    Eq -> doesCompMatchType bop t1 t2
    Neq -> doesCompMatchType bop t1 t2
    Gt -> doesCompMatchType bop t1 t2
    Ge -> doesCompMatchType bop t1 t2
    Lt -> doesCompMatchType bop t1 t2
    Le -> doesCompMatchType bop t1 t2
    Concat -> case (t1, t2) of
        (PrimitiveType StringType, PrimitiveType StringType) -> True
        _ -> False
    In -> case (t1, t2) of
        (PrimitiveType t1', PrimitiveType ObjectType) -> t1' == StringType || t1' == NumberType
        _ -> False

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
        Val value -> return (boolToCheckResult (doesUopMatchValue uop value))
        Var (Name name) -> do
            store <- S.get
            case store !? name of
                Just t' -> return (boolToCheckResult (doesUopMatchType uop t'))
                Nothing -> return Unknown
        _ -> return Unknown
doesExpressionMatchType (Op2 e1 bop e2) t =
    case (e1, e2) of
        (Val value1, Val value2) -> undefined -- TODO: check whether the given `bop` can be applied to the given `value1` and `value2`
        (Var (Name name1), Var (Name name2)) -> do
            store <- S.get
            case (store !? name1, store !? name2) of
                (Just t1, Just t2) -> return (boolToCheckResult (doesBopMatchType bop t1 t2))
                _ -> return Unknown
        (Var (Name name), Val value) -> do
            store <- S.get
            case store !? name of
                Just t' -> undefined -- TODO: check whether the given `bop` can be applied to the given `t'` and `value`
                Nothing -> return Unknown 
        (Val value, Var (Name name)) -> do
            store <- S.get
            case store !? name of
                Just t' -> undefined
                Nothing -> return Unknown
        _ -> return Unknown
doesExpressionMatchType (Call fn es) t = undefined

