module TypeChecker where
import Syntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Data.Map (Map)
import qualified Data.Map as Map

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
doesValueMatchType :: Value -> Type -> Bool
doesValueMatchType value (PrimitiveType t) = doesValueMatchPrimitiveType value t
doesValueMatchType value (UnionType types) = any (doesValueMatchPrimitiveType value) types
doesValueMatchType value (MaybeType t) = 
    doesValueMatchPrimitiveType value t 
    || doesValueMatchPrimitiveType value UndefinedType 
    || doesValueMatchPrimitiveType value NullType

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
