module FlowParser where

import Parser (Parser)
import Parser qualified as P
import Syntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

-- QC properties
prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_exp :: Expression -> Bool
prop_roundtrip_exp e = P.parse expP (pretty e) == Right e

prop_roundtrip_prim :: PrimitiveValue -> Bool
prop_roundtrip_prim p = P.parse primitiveP (pretty p) == Right p

-- Parsing
statementP :: Parser Statement
statementP = undefined

valueP :: Parser Value
valueP = undefined

expP :: Parser Expression -- type errors will be detected here
expP = undefined

primitiveP :: Parser PrimitiveValue
primitiveP = undefined

uopP :: Parser Uop
uopP = undefined

bopP :: Parser Bop
bopP = undefined

-- Parsing, helper funcs
expressionValid :: Expression -> Bool
expressionValid (Val val) = undefined
expressionValid (Op1 uop e) = undefined
expressionValid (Op2 e1 bop e2) = undefined