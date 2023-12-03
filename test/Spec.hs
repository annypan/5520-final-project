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

-- Any value can be used as AnyType
prop_all_value_match_any_type :: Value -> Bool
prop_all_value_match_any_type v = doesValueMatchType v AnyType

-- No value can be used as EmptyType
prop_no_value_match_empty_type :: Value -> Bool
prop_no_value_match_empty_type v = not (doesValueMatchType v EmptyType)

-- Any type can be used as AnyType
prop_all_type_match_any_type :: Type -> Bool
prop_all_type_match_any_type t = canBeUsedAsType t AnyType

-- A type can be used as a MaybeType of the same type
prop_type_as_maybe_type :: Type -> Bool
prop_type_as_maybe_type t = canBeUsedAsType t (MaybeType t)

-- A type can be used as itself
prop_type_as_itself :: Type -> QC.Property
prop_type_as_itself t = isPrimitive t QC.==> canBeUsedAsType t t

-- t1 <= t2, t2 <= t3 => t1 <= t3
prop_type_is_transitive :: Type -> Type -> Type -> QC.Property
prop_type_is_transitive t1 t2 t3 =
    canBeUsedAsType t1 t2 && canBeUsedAsType t2 t3 QC.==> canBeUsedAsType t1 t3

-- AnyType matches any uop
prop_anytype_matches_any_uop :: Uop -> Bool
prop_anytype_matches_any_uop uop = doesUopMatchType uop AnyType

-- AnyType matches any bop with any type
prop_anytype_matches_any_bop :: Bop -> Type -> Bool
prop_anytype_matches_any_bop bop = doesBopMatchType bop AnyType

-- if a bop matches a type, then it also matches the type in reverse order
prop_bop_match_is_commutative :: Bop -> Type -> Type -> Bool
prop_bop_match_is_commutative bop t1 t2 = doesBopMatchType bop t1 t2 == doesBopMatchType bop t2 t1

typecheckerAllQC :: IO ()
typecheckerAllQC = do
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
  putStrLn "prop_type_is_transitive"
  QC.quickCheck prop_type_is_transitive
  putStrLn "prop_anytype_matches_any_uop"
  QC.quickCheck prop_anytype_matches_any_uop
  putStrLn "prop_anytype_matches_any_bop"
  QC.quickCheck prop_anytype_matches_any_bop
  putStrLn "prop_bop_match_is_commutative"
  QC.quickCheck prop_bop_match_is_commutative

-- unit tests
test_unit_all :: IO Counts
test_unit_all =
    runTestTT $ TestList [testCanBeUsedAsType, testGetType, testDoesExpressionMatchType, testCheckStatement]


testCanBeUsedAsType :: Test
testCanBeUsedAsType =
    TestList
        [
            "BoolType can be used as BoolType" ~: canBeUsedAsType BoolType BoolType ~?= True,
            "BoolType can be used as AnyType" ~: canBeUsedAsType BoolType AnyType ~?= True,
            "NullType can be used as AnyType" ~: canBeUsedAsType NullType AnyType ~?= True,
            "BoolType can be used as MaybeType BoolType" ~: canBeUsedAsType BoolType (MaybeType BoolType) ~?= True,
            "NumberType can be used as UnionType [NumberType, StringType]" ~: canBeUsedAsType NumberType (UnionType [NumberType, StringType]) ~?= True,
            "MaybeType BoolType can NOT be used as BoolType" ~: canBeUsedAsType (MaybeType BoolType) BoolType ~?= False,
            "ObjecType can NOT be used as EmptyType" ~: canBeUsedAsType (ObjectType Map.empty) EmptyType ~?= False,
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
            getType (ObjectVal (Map.fromList [("x", NumberVal 1), ("y", StringVal "ans")]))
                ~?= ObjectType (Map.fromList [("x", NumberType), ("y", StringType)]),
            getType UndefinedVal ~?= UndefinedType,
            getType NullVal ~?= NullType
        ]

objectVar :: Expression
objectVar = Val (ObjectVal (Map.fromList [("x", NumberVal 1)]))

objectVarLayered :: Expression
objectVarLayered = Val (ObjectVal (Map.fromList [("x", NumberVal 1), ("y", ObjectVal (Map.fromList [("z", NumberVal 2)]))]))

testDoesExpressionMatchType :: Test
testDoesExpressionMatchType =
    TestList
    [
        S.evalState (doesExpressionMatchType (Val (BoolVal True)) BoolType) Map.empty ~?= Success,
        S.evalState (doesExpressionMatchType (Val (ObjectVal Map.empty)) BoolType) Map.empty ~?= Failure,
        S.evalState (doesExpressionMatchType (Var (Name "x")) BoolType) (Map.fromList [("x", BoolType)]) ~?= Success,
        S.evalState (doesExpressionMatchType (Var (Name "x")) BoolType) Map.empty ~?= Failure,
        S.evalState (doesExpressionMatchType (Var (Name "x")) (MaybeType BoolType)) (Map.fromList [("x", BoolType)]) ~?= Success,
        S.evalState (doesExpressionMatchType (Var (Name "x")) (UnionType [BoolType, NumberType])) (Map.fromList [("x", BoolType)]) ~?= Success,
        S.evalState (doesExpressionMatchType (Var (Name "x")) BoolType) (Map.fromList [("x", MaybeType NumberType)]) ~?= Failure,
        S.evalState (doesExpressionMatchType (Var (Dot objectVar "x")) NumberType) Map.empty ~?= Success,
        S.evalState (doesExpressionMatchType (Var (Dot objectVar "y")) (ObjectType (Map.fromList [("z", NumberType)]))) Map.empty ~?= Success,
        S.evalState (doesExpressionMatchType (Var (Dot (Var (Dot objectVar "y")) "z")) NumberType) Map.empty ~?= Success
    ]

testCheckStatement :: Test
testCheckStatement =
    TestList
    [
        S.evalState (checkStatement (Assign (Name "x") (Val (BoolVal True)))) Map.empty ~?= Success,
        S.evalState (checkStatement (Assign (Name "x") (Val (BoolVal True)))) (Map.fromList [("x", NumberType)]) ~?= Failure
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