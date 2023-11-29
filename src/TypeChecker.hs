module TypeChecker where
import Syntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import State (State)
import qualified State as S
import Data.Bool (bool)

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

doesExpressionMatchType :: Expression -> Type -> State TypeDeclaration CheckResult
doesExpressionMatchType (Val value) t = return (doesValueMatchType value t)
doesExpressionMatchType (Var var) t = 
    case var of
        Name name -> do
            store <- S.get
            case store !? name of
                Just t' -> return (boolToCheckResult True) -- TODO: helper function for whether t' can be used when t is expected
                Nothing -> return (boolToCheckResult True) -- no static type info, do not raise error
        Dot t' name -> undefined
        Proj t' e -> undefined
doesExpressionMatchType (Op1 uop e) t = 
    case e of
        Val value -> undefined -- TODO: check whether the given `uop` can be applied to the given `value`
        _ -> undefined
        -- _ -> do
        --     store <- S.get
        --     case store !? e of
        --         Just t' -> return (boolToCheckResult True) -- TODO: helper function for whether t' can be used when t is expected
        --         Nothing -> return (boolToCheckResult True) -- no static type info, do not raise error
doesExpressionMatchType (Op2 e1 bop e2) t =
    case (e1, e2) of
        (Val value1, Val value2) -> undefined -- TODO: check whether the given `bop` can be applied to the given `value1` and `value2`
        _ -> return (boolToCheckResult True)
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

-- unit tests
testDoesValueMatchPrimitiveType :: Test
testDoesValueMatchPrimitiveType = 
    TestList
        [
            "BoolVal matches BoolType" ~: doesValueMatchPrimitiveType (BoolVal True) BoolType ~?= True,
            "StringVal matches StringType" ~: doesValueMatchPrimitiveType (StringVal "hello") StringType ~?= True,
            "NumberVal matches NumberType" ~: doesValueMatchPrimitiveType (NumberVal 1) NumberType ~?= True,
            "ObjectVal matches ObjectType" ~: doesValueMatchPrimitiveType (ObjectVal Map.empty) ObjectType ~?= True,
            "UndefinedVal matches UndefinedType" ~: doesValueMatchPrimitiveType UndefinedVal UndefinedType ~?= True,
            "NullVal matches NullType" ~: doesValueMatchPrimitiveType NullVal NullType ~?= True,
            "AnyVal matches AnyType" ~: doesValueMatchPrimitiveType (BoolVal True) AnyType ~?= True,
            "BoolVal does not match StringType" ~: doesValueMatchPrimitiveType (BoolVal True) StringType ~?= False,
            "StringVal does not match NumberType" ~: doesValueMatchPrimitiveType (StringVal "hello") NumberType ~?= False,
            "NumberVal does not match BoolType" ~: doesValueMatchPrimitiveType (NumberVal 1) BoolType ~?= False,
            "ObjectVal does not match UndefinedType" ~: doesValueMatchPrimitiveType (ObjectVal Map.empty) UndefinedType ~?= False,
            "UndefinedVal does not match NullType" ~: doesValueMatchPrimitiveType UndefinedVal NullType ~?= False,
            "NullVal does not match BoolType" ~: doesValueMatchPrimitiveType NullVal BoolType ~?= False,
            "AnyVal does not match UndefinedType" ~: doesValueMatchPrimitiveType (BoolVal True) UndefinedType ~?= False
        ]
