import Lib
import Test.HUnit
import Test.QuickCheck qualified as QC
import Syntax
import Parser qualified as P
import FlowParser
import PrettyPrinter
import qualified Data.Map as Map

import qualified Data.Set as Set
import TypeChecker
import State (State)
import qualified State as S

-- QC properties
prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_type :: Type -> Bool -- TODO: debug
prop_roundtrip_type t = P.parse typeP (pretty t) == Right t

prop_roundtrip_exp :: Expression -> Bool -- TODO: add Arbitrary for `Expression`
prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

-- if a value is a primitive type, then it can be used as a composite type; vice versa
prop_prim_as_composite_type :: Value -> PrimitiveType -> Bool
prop_prim_as_composite_type v pt = 
  boolToCheckResult (doesValueMatchPrimitiveType v pt) == doesValueMatchType v (PrimitiveType pt)

-- Any value can be used as AnyType
prop_all_value_match_any_type :: Value -> Bool
prop_all_value_match_any_type v = doesValueMatchType v (PrimitiveType AnyType) == Success

-- No value can be used as EmptyType
prop_no_value_match_empty_type :: Value -> Bool
prop_no_value_match_empty_type v = doesValueMatchType v (PrimitiveType EmptyType) == Failure

-- Any type can be used as AnyType
prop_all_type_match_any_type :: Type -> Bool
prop_all_type_match_any_type t = canBeUsedAsType t (PrimitiveType AnyType)

-- A primitive type can be used as a MaybeType of the same type
prop_type_as_maybe_type :: PrimitiveType -> Bool
prop_type_as_maybe_type t = canBeUsedAsType (PrimitiveType t) (MaybeType t)

-- A type can be used as itself
prop_type_as_itself :: Type -> Bool
prop_type_as_itself t = canBeUsedAsType t t

-- t1 <= t2, t2 <= t3 => t1 <= t3

-- AnyType matches any uop
prop_anytype_matches_any_uop :: Uop -> Bool
prop_anytype_matches_any_uop uop = doesUopMatchType uop (PrimitiveType AnyType)

-- AnyType matches any bop with any type
prop_anytype_matches_any_bop :: Bop -> Type -> Bool
prop_anytype_matches_any_bop bop = doesBopMatchType bop (PrimitiveType AnyType)

-- if a bop matches a type, then it also matches the type in reverse order
prop_bop_match_is_commutative :: Bop -> Type -> Type -> Bool
prop_bop_match_is_commutative bop t1 t2 = doesBopMatchType bop t1 t2 == doesBopMatchType bop t2 t1

typecheckerAllQC :: IO ()
typecheckerAllQC = do
  putStrLn "prop_prim_as_composite_type"
  QC.quickCheck prop_prim_as_composite_type
  putStrLn "prop_all_value_match_any_type"
  QC.quickCheck prop_all_value_match_any_type
  putStrLn "prop_no_value_match_empty_type"
  QC.quickCheck prop_no_value_match_empty_type
  putStrLn "prop_all_type_match_any_type"
  QC.quickCheck prop_all_type_match_any_type
  putStrLn "prop_type_as_maybe_type"
  QC.quickCheck prop_type_as_maybe_type
  putStrLn "prop_type_as_itself"
  QC.quickCheck prop_type_as_itself
  putStrLn "prop_anytype_matches_any_uop"
  QC.quickCheck prop_anytype_matches_any_uop
  putStrLn "prop_anytype_matches_any_bop"
  QC.quickCheck prop_anytype_matches_any_bop
  putStrLn "prop_bop_match_is_commutative"
  QC.quickCheck prop_bop_match_is_commutative

-- unit tests
test_unit_all :: IO Counts
test_unit_all = 
    runTestTT $ TestList [testDoesValueMatchPrimitiveType, testCanBeUsedAsType, testGetType, testDoesExpressionMatchType, testCheckStatement]

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
            "ObjectVal does not match UndefinedType" ~: doesValueMatchPrimitiveType (ObjectVal Map.empty) UndefinedType ~?= False,
            "UndefinedVal does not match NullType" ~: doesValueMatchPrimitiveType UndefinedVal NullType ~?= False,
            "NullVal does not match BoolType" ~: doesValueMatchPrimitiveType NullVal BoolType ~?= False,
            "AnyVal does not match UndefinedType" ~: doesValueMatchPrimitiveType (BoolVal True) UndefinedType ~?= False
        ]

testCanBeUsedAsType :: Test
testCanBeUsedAsType = 
    TestList
        [
            "BoolType can be used as BoolType" ~: canBeUsedAsType (PrimitiveType BoolType) (PrimitiveType BoolType) ~?= True,
            "BoolType can be used as AnyType" ~: canBeUsedAsType (PrimitiveType BoolType) (PrimitiveType AnyType) ~?= True,
            "NullType can be used as AnyType" ~: canBeUsedAsType (PrimitiveType NullType) (PrimitiveType AnyType) ~?= True,
            "BoolType can be used as MaybeType BoolType" ~: canBeUsedAsType (PrimitiveType BoolType) (MaybeType BoolType) ~?= True,
            "NumberType can be used as UnionType [NumberType, StringType]" ~: canBeUsedAsType (PrimitiveType NumberType) (UnionType [NumberType, StringType]) ~?= True,
            "MaybeType BoolType can NOT be used as BoolType" ~: canBeUsedAsType (MaybeType BoolType) (PrimitiveType BoolType) ~?= False,
            "ObjecType can NOT be used as EmptyType" ~: canBeUsedAsType (PrimitiveType ObjectType) (PrimitiveType EmptyType) ~?= False,
            "MaybeType BoolType can be used as UnionType [BoolType, UndefinedType, NullType]" ~: canBeUsedAsType (MaybeType BoolType) (UnionType [BoolType, UndefinedType, NullType]) ~?= True,
            "UnionType [BoolType, NullType] can be used as MaybeType BoolType" ~: canBeUsedAsType (UnionType [BoolType, UndefinedType, NullType]) (MaybeType BoolType) ~?= True
        ]

testGetType :: Test
testGetType = 
    TestList
        [
            getType (BoolVal True) ~?= BoolType,
            getType (StringVal "hello") ~?= StringType,
            getType (NumberVal 1) ~?= NumberType,
            getType (ObjectVal Map.empty) ~?= ObjectType,
            getType UndefinedVal ~?= UndefinedType,
            getType NullVal ~?= NullType
        ]

testDoesExpressionMatchType :: Test
testDoesExpressionMatchType = 
    TestList
    [
        S.evalState (doesExpressionMatchType (Val (BoolVal True)) (PrimitiveType BoolType)) Map.empty ~?= Success,
        S.evalState (doesExpressionMatchType (Val (ObjectVal Map.empty)) (PrimitiveType BoolType)) Map.empty ~?= Failure,
        S.evalState (doesExpressionMatchType (Var (Name "x")) (PrimitiveType BoolType)) (Map.fromList [("x", PrimitiveType BoolType)]) ~?= Success,
        S.evalState (doesExpressionMatchType (Var (Name "x")) (PrimitiveType BoolType)) Map.empty ~?= Unknown,
        S.evalState (doesExpressionMatchType (Var (Name "x")) (MaybeType BoolType)) (Map.fromList [("x", PrimitiveType BoolType)]) ~?= Success,
        S.evalState (doesExpressionMatchType (Var (Name "x")) (UnionType [BoolType, NumberType])) (Map.fromList [("x", PrimitiveType BoolType)]) ~?= Success,
        S.evalState (doesExpressionMatchType (Var (Name "x")) (PrimitiveType BoolType)) (Map.fromList [("x", MaybeType NumberType)]) ~?= Failure
    ]

testCheckStatement :: Test
testCheckStatement = 
    TestList
    [
        S.evalState (checkStatement (Assign (Name "x") (Val (BoolVal True)))) Map.empty ~?= Success,
        S.evalState (checkStatement (Assign (Name "x") (Val (BoolVal True)))) (Map.fromList [("x", PrimitiveType NumberType)]) ~?= Failure
    ]

main :: IO ()
main = do
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_val
--   putStrLn "roundtrip_type"
--   QC.quickCheck prop_roundtrip_type
--   putStrLn "roundtrip_exp"
--   QC.quickCheck prop_roundtrip_exp
  putStrLn "\n**typechecker qc tests**\n"
  typecheckerAllQC
  putStrLn "\n**unit tests**\n"
  _ <- test_unit_all
  return ()