import FlowParser
import Lib
import Test.HUnit
import Test.QuickCheck

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