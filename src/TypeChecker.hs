module TypeChecker where
import Syntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import State (State)
import qualified State as S
import qualified Data.Set as Set
import FlowParser
import Data.Maybe (isJust)
import PrettyPrinter

type TypeDeclaration = Map Name Type

data CheckResult
  = Success
  | Failure String
  | Unknown
  deriving (Eq, Show)

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

-- Unwraps nested maybe types
unwrapMaybeType :: Type -> Type
unwrapMaybeType (MaybeType t) = unwrapMaybeType t
unwrapMaybeType t = t

-- Checks if a type can be used when another type is expected
canBeUsedAsType :: Type -> Type -> Bool
canBeUsedAsType _ AnyType = True
canBeUsedAsType (UnionType ts1) (UnionType ts2) =
    Set.fromList ts1 `Set.isSubsetOf` Set.fromList ts2
    || AnyType `elem` ts2
canBeUsedAsType t1@(UnionType ts1) (MaybeType t2) =
    Set.fromList ts1 `Set.isSubsetOf` Set.fromList [t2, UndefinedType, NullType]
    || t2 == AnyType
    || t1 == t2
canBeUsedAsType (MaybeType t1) (UnionType ts2) =
    Set.fromList [t1, UndefinedType, NullType] `Set.isSubsetOf` Set.fromList ts2
    || AnyType `elem` ts2
canBeUsedAsType (MaybeType t1) (MaybeType t2) =
    let t1' = unwrapMaybeType t1
        t2' = unwrapMaybeType t2
    in t1' == t2' || t1' == UndefinedType || t1' == NullType || t2' == AnyType
canBeUsedAsType t1 (UnionType ts) = t1 `elem` ts || AnyType `elem` ts
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

-- Gets the return type of a unary operator, assuming the input type is valid
getUopReturnType :: Uop -> Type -> Type
getUopReturnType Neg t = t
getUopReturnType Not _ = BoolType
getUopReturnType TypeOf _ = StringType

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

-- Gets the return type of a binary operator, assuming the input types are valid
getBopReturnType :: Bop -> Type -> Type -> Type
getBopReturnType bop t1 t2 = case bop of
    Plus -> t1
    Minus -> t1
    Times -> t1
    Divide -> t1
    Modulo -> t1
    Eq -> BoolType
    Neq -> BoolType
    Gt -> BoolType
    Ge -> BoolType
    Lt -> BoolType
    Le -> BoolType
    Concat -> StringType
    In -> BoolType

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
                Just (ObjectType obj) -> return (obj !? name)
                _ -> return Nothing
        _ -> return Nothing
resolveVarType (Proj exp name) = resolveVarType (Dot exp name)

-- Gets a list of types from a list of maybe types
getValidTypeList :: [Maybe Type] -> Maybe [Type]
getValidTypeList [] = Just []
getValidTypeList (Just t : ts) = case getValidTypeList ts of
    Just ts' -> Just (t : ts')
    Nothing -> Nothing
getValidTypeList (Nothing : ts) = Nothing

-- Synthesizes the type of an expression
synthesizeType :: Expression -> State TypeDeclaration (Maybe Type)
synthesizeType (Val value) = return (Just (getType value))
synthesizeType (Var var) = do
    res <- resolveVarType var
    case res of
        Just t -> return (Just t)
        Nothing -> return Nothing
synthesizeType (Op1 uop e) = do
    t <- synthesizeType e
    case t of
        Just t' -> return (
            if doesUopMatchType uop t' then Just (getUopReturnType uop t') else Nothing)
        Nothing -> return Nothing
synthesizeType (Op2 e1 bop e2) = do
    t1 <- synthesizeType e1
    t2 <- synthesizeType e2
    case (t1, t2) of
        (Just t1', Just t2') -> return (
            if doesBopMatchType bop t1' t2' then Just (getBopReturnType bop t1' t2') else Nothing)
        _ -> return Nothing
synthesizeType (Call fn es) = do
    store <- S.get
    case store !? fn of
        Just (FunctionType args ret) -> do
            ts <- mapM synthesizeType es
            let sanitizedTs = getValidTypeList ts
            case sanitizedTs of
                Just sanitizedTs' ->
                    return (
                        if all (uncurry canBeUsedAsType) (zip args sanitizedTs')
                        then Just ret
                        else Nothing)
                Nothing -> return Nothing
        _ -> return Nothing

-- Checks if an expression can be used as a given type
doesExpressionMatchType :: Expression -> Type -> State TypeDeclaration CheckResult
doesExpressionMatchType e t = do
    t' <- synthesizeType e
    case t' of
        Just t'' -> (if canBeUsedAsType t'' t then return Success else return (Failure (pretty e)))
        Nothing -> return (Failure (pretty e))

-- Checks if a block is valid
checkBlock :: Block -> State TypeDeclaration [CheckResult]
checkBlock (Block statements) = do
    results <- mapM checkStatement statements
    return (concat results)

-- Checks if a statement is valid
checkStatement :: Statement -> State TypeDeclaration [CheckResult]
checkStatement s@(Assign var e) = do
    store <- S.get
    varType <- resolveVarType var
    case varType of
        Just t -> do
           res <- doesExpressionMatchType e t
           return [(\r -> if r == Success then Success else Failure (pretty s)) res]
        Nothing ->
            case var of
                Name name -> do
                    expType <- synthesizeType e
                    case expType of
                        Just t -> do
                            S.put (Map.insert name t store)
                            return [Success]
                        Nothing -> return [Failure (pretty s)]
                _ -> return [Unknown]
checkStatement s@(Update var e) = do
    store <- S.get
    varType <- resolveVarType var
    case varType of
        Just t -> do
            res <- doesExpressionMatchType e t
            return [(\r -> if r == Success then Success else Failure (pretty s)) res]
        Nothing -> return [Failure (pretty s)] -- var must have been declared; otherwise, it's an error
checkStatement s@(If e s1 s2) = do
    re <- synthesizeType e
    r1 <- checkBlock s1
    r2 <- checkBlock s2
    return ((if isJust re then Success else Failure (pretty e)): r1 ++ r2);
checkStatement st@(While e s) = do
    re <- synthesizeType e
    res <- checkBlock s
    return ((if isJust re then Success else Failure (pretty e)): res)
checkStatement Empty = return [Success]
checkStatement s@(For s1 e1 e2 s2) = case s1 of
    Assign _ _ -> do
        t1 <- synthesizeType e1
        case t1 of
            Just BoolType -> do -- e1 is the stop condition: must be bool
                checkBlock s2
            _ -> return [Failure (pretty s)]
    _ -> return [Failure (pretty s)]
checkStatement (Return e) = return [Success]
checkStatement st@(FunctionDef name t s) = case t of
    FunctionType args ret -> do
        store <- S.get
        S.put (Map.insert name t store)
        -- TODO: check if the actual return type matches the declared return type
        r <- checkBlock s
        S.put store
        return r
    _ -> return [Failure (pretty s)]

-- Runs a block and returns the results and the updated type declaration
runBlock :: Block -> State TypeDeclaration ([CheckResult], TypeDeclaration)
runBlock (Block statements) = do
    results <- mapM checkStatement statements
    store <- S.get
    return (concat results, store)

initialStore :: TypeDeclaration
initialStore = Map.empty

-- Checks the js file and returns the results
checker :: String -> IO ([CheckResult], TypeDeclaration)
checker s = do
    res <- parseJSFile s
    case res of
        Left err -> print err >> return ([], initialStore)
        Right (Block statements) -> return (S.evalState (runBlock (Block statements)) initialStore)
