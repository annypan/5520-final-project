module TypeChecker where
import Syntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import State (State)
import qualified State as S
import qualified Data.Set as Set
import FlowParser

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

-- Checks if a type is a primitive type
isPrimitive :: Type -> Bool
isPrimitive BoolType = True
isPrimitive StringType = True
isPrimitive NumberType = True
isPrimitive NullType = True
isPrimitive UndefinedType = True
isPrimitive _ = False

-- Checks if a value can be used as a given type
doesValueMatchType :: Value -> Type -> Bool
doesValueMatchType (BoolVal _) BoolType = True
doesValueMatchType (StringVal _) StringType = True
doesValueMatchType (NumberVal _) NumberType = True
doesValueMatchType UndefinedVal UndefinedType = True
doesValueMatchType NullVal NullType = True
doesValueMatchType _ AnyType = True
doesValueMatchType value (UnionType types) =
    any (doesValueMatchType value) types
doesValueMatchType value (MaybeType t) =
    doesValueMatchType value t
        || doesValueMatchType value UndefinedType
        || doesValueMatchType value NullType
doesValueMatchType (ObjectVal obj) (ObjectType typeMap)
    = all (\(name, value) -> case typeMap !? name of
        Just t -> doesValueMatchType value t
        Nothing -> False
        ) (Map.toList obj)
doesValueMatchType value (FunctionType args ret) = False
doesValueMatchType _ _ = False

unwrapMaybeType :: Type -> Type
unwrapMaybeType (MaybeType t) = unwrapMaybeType t
unwrapMaybeType t = t

-- Checks if a type can be used when another type is expected
canBeUsedAsType :: Type -> Type -> Bool
canBeUsedAsType _ AnyType = True
canBeUsedAsType (UnionType ts1) (UnionType ts2) =
    Set.fromList ts1 `Set.isSubsetOf` Set.fromList ts2
canBeUsedAsType (UnionType ts1) (MaybeType t2) =
    Set.fromList ts1 `Set.isSubsetOf` Set.fromList [t2, UndefinedType, NullType]
canBeUsedAsType (MaybeType t1) (UnionType ts2) =
    Set.fromList [t1, UndefinedType, NullType] `Set.isSubsetOf` Set.fromList ts2
canBeUsedAsType (MaybeType t1) (MaybeType t2) = 
    let t1' = unwrapMaybeType t1
        t2' = unwrapMaybeType t2
    in t1' == t2' || t1' == UndefinedType || t1' == NullType || t2' == AnyType
canBeUsedAsType t1 (UnionType ts) = t1 `elem` ts
canBeUsedAsType t1 (MaybeType t2) = t1 == t2 || t1 == UndefinedType || t1 == NullType || t2 == AnyType
canBeUsedAsType (FunctionType args1 ret1) (FunctionType args2 ret2) =
    args1 == args2 && ret1 == ret2
canBeUsedAsType (ObjectType t1) (ObjectType t2) = Map.isSubmapOf t1 t2
canBeUsedAsType t1 t2 = t1 == t2

-- Gets the type of a value
getType :: Value -> Type
getType (BoolVal _) = BoolType
getType (StringVal _) = StringType
getType (NumberVal _) = NumberType
getType (ObjectVal obj) = ObjectType (Map.map getType obj)
getType UndefinedVal = UndefinedType
getType NullVal = NullType

-- Checks if a unary operator can be used with a given type
doesUopMatchType :: Uop -> Type -> Bool
doesUopMatchType Neg (UnionType ts) =
    Set.fromList ts `Set.isSubsetOf` Set.fromList [NumberType, AnyType]
doesUopMatchType Neg t = t == NumberType || t == AnyType
doesUopMatchType Not t = True
doesUopMatchType TypeOf _ = True

-- Checks if a unary operator can be used with a given value
doesUopMatchValue :: Uop -> Value -> Bool
doesUopMatchValue uop val = doesUopMatchType uop (getType val)

-- Checks if an arithmetic operator can be used with given types
doesArithMatchType :: Bop -> Type -> Type -> Bool
doesArithMatchType arith t1 t2 = case (t1, t2) of
    (AnyType, _) -> True
    (_, AnyType) -> True
    (NumberType, NumberType) -> True
    (UnionType ts1, UnionType ts2) ->
        all (\t1 -> all (doesArithMatchType arith t1) ts2) ts1
    _ -> False

-- Checks if a comparison operator can be used with given types
doesCompMatchType :: Bop -> Type -> Type -> Bool
doesCompMatchType bop t1 t2 = case (t1, t2) of
    (AnyType, _) -> True
    (_, AnyType) -> True
    (NumberType, NumberType) -> True
    (StringType, StringType) -> True
    (UnionType ts1, UnionType ts2) ->
        all (\t1 -> all (doesCompMatchType bop t1) ts2) ts1
    _ -> False

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
        (AnyType, _) -> True
        (_, AnyType) -> True
        (StringType, StringType) -> True
        _ -> False
    In -> case (t1, t2) of
        (AnyType, _) -> True
        (_, AnyType) -> True
        (StringType, ObjectType _) -> True
        _ -> False

-- Checks if a binary operator can be used with given values
doesBopMatchValue :: Bop -> Value -> Value -> Bool
doesBopMatchValue bop v1 v2 = doesBopMatchType bop (getType v1) (getType v2)

-- Resolves the type of a variable
resolveVarType :: Var -> State TypeDeclaration (Maybe Type)
resolveVarType (Name name) = do
    store <- S.get
    case store !? name of
        Just t -> return (Just t)
        Nothing -> return Nothing
resolveVarType (Dot exp name) = do
    store <- S.get
    case exp of
        Val (ObjectVal obj) ->
            case obj !? name of
                Just v -> return (Just (getType v))
                Nothing -> return Nothing
        Var var -> do
            t <- resolveVarType var
            case t of
                Just (ObjectType obj) ->
                    case obj !? name of
                        Just t -> return (Just t)
                        Nothing -> return Nothing
                _ -> return Nothing
        _ -> return Nothing
resolveVarType (Proj exp name) = resolveVarType (Dot exp name)

-- Checks if an expression can be used as a given type
doesExpressionMatchType :: Expression -> Type -> State TypeDeclaration CheckResult
doesExpressionMatchType (Val value) t = return (boolToCheckResult (doesValueMatchType value t))
doesExpressionMatchType (Var var) t = do
    res <- resolveVarType var
    case res of
        Just t' -> return (boolToCheckResult (canBeUsedAsType t' t))
        Nothing -> return Failure
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
        (Val value1, Val value2) -> return (boolToCheckResult (doesBopMatchValue bop value1 value2))
        (Var (Name name1), Var (Name name2)) -> do
            store <- S.get
            case (store !? name1, store !? name2) of
                (Just t1, Just t2) -> return (boolToCheckResult (doesBopMatchType bop t1 t2))
                _ -> return Unknown
        (Var (Name name), Val value) -> do
            store <- S.get
            case store !? name of
                Just t' -> return (boolToCheckResult (doesBopMatchType bop t' (getType value)))
                Nothing -> return Unknown
        (Val value, Var (Name name)) -> do
            store <- S.get
            case store !? name of
                Just t' -> undefined
                Nothing -> return Unknown
        _ -> return Unknown
doesExpressionMatchType (Call fn es) t = undefined

initialStore :: TypeDeclaration
initialStore = Map.empty

checkStatement :: Statement -> State TypeDeclaration CheckResult
checkStatement (Assign var e) = do
    store <- S.get
    case var of
        Name name -> do
            case store !? name of
                Just t -> do
                    result <- doesExpressionMatchType e t -- t already exists, check if e matches t
                    case e of
                        Val value -> do
                            S.put (Map.insert name (getType value) store)
                            return result
                        _ -> return result
                Nothing ->
                    case e of
                        Val value -> do
                            S.put (Map.insert name (getType value) store)
                            return Success
                        _ -> return Success
        _ -> return Unknown
checkStatement _ = undefined

checker :: String -> IO ([CheckResult], TypeDeclaration)
checker s = do
    res <- parseJSFile s
    case res of
        Left err -> print err >> return ([], initialStore)
        Right (Block statements) ->
            let (results, td) = S.runState (mapM checkStatement statements) initialStore
            in return (results, td)
