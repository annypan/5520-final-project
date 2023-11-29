import Lib
import Test.HUnit
import Test.QuickCheck qualified as QC
import Syntax
import Parser qualified as P
import FlowParser
import PrettyPrinter
import qualified Data.Map as Map
import TypeChecker

-- QC properties
prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
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

-- unit tests
test_unit_all :: IO Counts
test_unit_all = runTestTT $ TestList [testDoesValueMatchPrimitiveType, testCanBeUsedAsType]

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

testCanBeUsedAsType :: Test
testCanBeUsedAsType = 
    TestList
        [
            "BoolType can be used as BoolType" ~: canBeUsedAsType (PrimitiveType BoolType) (PrimitiveType BoolType) ~?= True,
            "ObjectType can be used as BoolType" ~: canBeUsedAsType (PrimitiveType ObjectType) (PrimitiveType BoolType) ~?= True
        ]

main :: IO ()
main = do
  -- putStrLn "roundtrip_val"
  -- QC.quickCheck prop_roundtrip_val
  -- putStrLn "roundtrip_exp"
  -- QC.quickCheck prop_roundtrip_exp
  putStrLn "prop_prim_as_composite_type"
  QC.quickCheck prop_prim_as_composite_type
  putStrLn "prop_all_value_match_any_type"
  QC.quickCheck prop_all_value_match_any_type
  putStrLn "prop_no_value_match_empty_type"
  QC.quickCheck prop_no_value_match_empty_type
  putStrLn "\n**unit tests**\n"
  _ <- test_unit_all
  return ()