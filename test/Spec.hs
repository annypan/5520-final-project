import Lib
import Test.HUnit
import Test.QuickCheck qualified as QC

-- QC properties
prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (pretty e) == Right e


main :: IO ()
main = do
  putStrLn someFunc
  putStrLn "Test suite not yet implemented"
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_val
  putStrLn "roundtrip_exp"
  QC.quickCheck prop_roundtrip_exp
  putStrLn "roundtrip_prim"
  QC.quickCheck prop_roundtrip_prim