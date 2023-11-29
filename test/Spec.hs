import Lib
import Test.HUnit
import Test.QuickCheck qualified as QC
import Syntax
import Parser qualified as P
import FlowParser
import PrettyPrinter
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

main :: IO ()
main = do
  -- putStrLn "roundtrip_val"
  -- QC.quickCheck prop_roundtrip_val
  -- putStrLn "roundtrip_exp"
  -- QC.quickCheck prop_roundtrip_exp
  putStrLn "prop_prim_as_composite_type"
  QC.quickCheck prop_prim_as_composite_type