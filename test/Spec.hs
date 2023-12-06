import ASTExamples
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import FlowParser
import Lib
import Parser qualified as P
import PrettyPrinter
import State (State)
import State qualified as S
import Syntax
import Test.HUnit
import Test.QuickCheck qualified as QC
import TypeChecker

isNotFuncType :: Type -> Bool
isNotFuncType (FunctionType _ _) = False
isNotFuncType _ = True

-- QC properties
prop_roundtrip_val :: Value -> Bool
prop_roundtrip_val v = P.parse valueP (pretty v) == Right v

prop_roundtrip_type :: Type -> QC.Property -- don't test for func type because we ignore some info when parsing
prop_roundtrip_type t = isNotFuncType t QC.==> P.parse typeP (pretty t) == Right t

prop_roundtrip_exp :: Expression -> Bool
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

prop_type_as_itself :: Type -> Bool
prop_type_as_itself t = canBeUsedAsType t t

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

isNotIn :: Bop -> Bool
isNotIn In = False
isNotIn _ = True

-- if a bop matches a type, then it also matches the type in reverse order (except IN)
prop_bop_match_is_commutative :: Bop -> Type -> Type -> QC.Property
prop_bop_match_is_commutative bop t1 t2 = isNotIn bop QC.==> doesBopMatchType bop t1 t2 == doesBopMatchType bop t2 t1

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
test_parser_unit_all :: IO Counts
test_parser_unit_all =
  runTestTT $
    TestList
      [tParseFiles]

tParseFiles :: Test
tParseFiles =
  "parse files" ~:
    TestList
      [ "assign" ~: p "js/assign.js" wAssign,
        "assignConflict" ~: p "js/assignConflict.js" wAssignConflict,
        "if" ~: p "js/if.js" wIf,
        "updateConflict" ~: p "js/updateConflict.js" wUpdateConflict,
        "ifLiteral" ~: p "js/ifLiteral.js" wIfLiteral,
        "ifBranchConflict" ~: p "js/ifBranchConflict.js" wIfBranchConflict,
        "while" ~: p "js/while.js" wWhile,
        "whileCondConflict" ~: p "js/whileCondConflict.js" wWhileCondConflict,
        "for" ~: p "js/for.js" wFor,
        "forWrongVar" ~: p "js/forWrongVar.js" wForWrongVar,
        "forNoAssign" ~: p "js/forNoAssign.js" wForNoAssign,
        "functionDef" ~: p "js/functionDef.js" wFunctionDef
      ]
  where
    p fn ast = do
      result <- parseJSFile fn
      case result of
        Left _ -> assert False
        Right ast' -> assert (ast == ast')

test_checker_unit_all :: IO Counts
test_checker_unit_all =
    runTestTT $ TestList [testCanBeUsedAsType, testGetType, testSynthesizeType, testResolveVarType,
                          testDoesExpressionMatchType, testCheckStatement, testRunBlock]

testCanBeUsedAsType :: Test
testCanBeUsedAsType =
  TestList
    [ "BoolType can be used as BoolType" ~: canBeUsedAsType BoolType BoolType ~?= True,
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
    [ getType (BoolVal True) ~?= BoolType,
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

objectTypeLayered :: Type
objectTypeLayered = ObjectType (Map.fromList [("x", NumberType), ("y", ObjectType (Map.fromList [("z", NumberType)]))])

mapLayered :: Map String Type
mapLayered = Map.fromList [("x", NumberType), ("y", ObjectType (Map.fromList [("z", NumberType)]))]

testResolveVarType :: Test
testResolveVarType =
  TestList
    [ S.evalState (resolveVarType (Name "x")) (Map.fromList [("x", NumberType)]) ~?= Just NumberType,
      S.evalState (resolveVarType (Dot (Var (Name "y")) "z")) mapLayered ~?= Just NumberType,
      S.evalState (resolveVarType (Proj (Var (Name "y")) "z")) mapLayered ~?= Just NumberType,
      S.evalState (resolveVarType (Name "k")) mapLayered ~?= Nothing
    ]

convertToBool :: CheckResult -> Bool
convertToBool Success = True
convertToBool (Failure _) = False

testDoesExpressionMatchType :: Test
testDoesExpressionMatchType =
    TestList
    [
        convertToBool (S.evalState (doesExpressionMatchType (Val (BoolVal True)) BoolType) Map.empty) ~?= True,
        convertToBool (S.evalState (doesExpressionMatchType (Val (ObjectVal Map.empty)) BoolType) Map.empty) ~?= False,
        convertToBool (S.evalState (doesExpressionMatchType (Var (Name "x")) BoolType) (Map.fromList [("x", BoolType)])) ~?= True,
        convertToBool (S.evalState (doesExpressionMatchType (Var (Name "x")) BoolType) Map.empty) ~?= False,
        convertToBool (S.evalState (doesExpressionMatchType (Var (Name "x")) (MaybeType BoolType)) (Map.fromList [("x", BoolType)])) ~?= True,
        convertToBool (S.evalState (doesExpressionMatchType (Var (Name "x")) (UnionType [BoolType, NumberType])) (Map.fromList [("x", BoolType)])) ~?= True,
        convertToBool (S.evalState (doesExpressionMatchType (Var (Name "x")) BoolType) (Map.fromList [("x", MaybeType NumberType)])) ~?= False,
        convertToBool (S.evalState (doesExpressionMatchType (Var (Dot objectVar "x")) NumberType) Map.empty) ~?= True,
        convertToBool (S.evalState (doesExpressionMatchType (Var (Dot objectVarLayered "y")) (ObjectType (Map.fromList [("z", NumberType)]))) Map.empty) ~?= True,
        convertToBool (S.evalState (doesExpressionMatchType (Var (Dot (Var (Dot objectVarLayered "y")) "z")) NumberType) Map.empty) ~?= True
    ]

testSynthesizeType :: Test
testSynthesizeType =
  TestList
    [ S.evalState (synthesizeType (Val (BoolVal True))) Map.empty ~?= Just BoolType,
      S.evalState (synthesizeType (Val (ObjectVal Map.empty))) Map.empty ~?= Just (ObjectType Map.empty),
      S.evalState (synthesizeType (Var (Name "x"))) (Map.fromList [("x", BoolType)]) ~?= Just BoolType,
      S.evalState (synthesizeType (Var (Name "x"))) Map.empty ~?= Nothing,
      S.evalState (synthesizeType (Var (Name "x"))) (Map.fromList [("x", MaybeType NumberType)]) ~?= Just (MaybeType NumberType),
      S.evalState (synthesizeType (Var (Name "x"))) (Map.fromList [("x", UnionType [BoolType, NumberType])]) ~?= Just (UnionType [BoolType, NumberType]),
      S.evalState (synthesizeType (Var (Name "x"))) (Map.fromList [("x", MaybeType NumberType)]) ~?= Just (MaybeType NumberType),
      S.evalState (synthesizeType (Var (Dot objectVar "x"))) Map.empty ~?= Just NumberType,
      S.evalState (synthesizeType (Var (Dot objectVarLayered "y"))) Map.empty ~?= Just (ObjectType (Map.fromList [("z", NumberType)])),
      S.evalState (synthesizeType (Var (Dot (Var (Dot objectVarLayered "y")) "z"))) Map.empty ~?= Just NumberType,
      S.evalState (synthesizeType (Op1 Not (Val (BoolVal True)))) Map.empty ~?= Just BoolType,
      S.evalState (synthesizeType (Op1 Neg (Val (BoolVal True)))) Map.empty ~?= Nothing,
      S.evalState (synthesizeType (Op1 Neg (Val (NumberVal 1)))) Map.empty ~?= Just NumberType,
      S.evalState (synthesizeType (Op1 Neg (Var (Name "x")))) (Map.fromList [("x", NumberType)]) ~?= Just NumberType,
      S.evalState (synthesizeType (Op1 Neg (Var (Name "x")))) (Map.fromList [("x", MaybeType NumberType)]) ~?= Nothing,
      S.evalState (synthesizeType (Op2 (Val (NumberVal 1)) Plus (Val (NumberVal 2)))) Map.empty ~?= Just NumberType,
      S.evalState (synthesizeType (Op2 (Val (NumberVal 1)) Plus (Val (BoolVal True)))) Map.empty ~?= Nothing,
      S.evalState (synthesizeType (Call "f" [Val (NumberVal 1), Val (BoolVal True)])) (Map.fromList [("f", FunctionType [NumberType, BoolType] NumberType)]) ~?= Just NumberType
    ]

testCheckStatement :: Test
testCheckStatement =
  TestList
    [ convertToBool <$> S.evalState (checkStatement (Assign (Name "x") (Val (BoolVal True)))) Map.empty ~?= [True],
      convertToBool <$> S.evalState (checkStatement (Assign (Name "x") (Val (BoolVal True)))) (Map.fromList [("x", NumberType)]) ~?= [False]
    ]

convertFstToBool :: ([CheckResult], Map String Type) -> ([Bool], Map String Type)
convertFstToBool (res, s) = (convertToBool <$> res, s)

testRunBlock :: Test
testRunBlock =
  TestList
    [ convertFstToBool (
        S.evalState (runBlock (Block [Assign (Name "x") (Val (BoolVal True)), Assign (Name "x") (Val (NumberVal 1))])) Map.empty
      ) ~?= ([True, False], Map.fromList [("x", BoolType)]),
      convertFstToBool (
        S.evalState (runBlock (Block [Assign (Name "x") (Val (BoolVal True))])) (Map.fromList [("x", NumberType)])
      ) ~?= ([False], Map.fromList [("x", NumberType)]),
      convertFstToBool (S.evalState
        ( runBlock
            ( Block
                [ Assign (Name "x") (Val (ObjectVal (Map.fromList [("y", NumberVal 1), ("z", StringVal "str")]))),
                  Assign (Name "x") (Val (NumberVal 1))
                ]
            )
        )
        Map.empty)
        ~?= ([True, False], Map.fromList [("x", ObjectType (Map.fromList [("y", NumberType), ("z", StringType)]))]),
      convertFstToBool (S.evalState
        ( runBlock
            ( Block
                [ Assign (Name "x") (Val (ObjectVal (Map.fromList [("y", NumberVal 1), ("z", StringVal "str")]))),
                  Assign (Dot (Var (Name "x")) "y") (Val (StringVal "str"))
                ]
            )
        )
        Map.empty)
        ~?= ([True, False], Map.fromList [("x", ObjectType (Map.fromList [("y", NumberType), ("z", StringType)]))])
    ]

main :: IO ()
main = do
  putStrLn "roundtrip_val"
  QC.quickCheck prop_roundtrip_val
  putStrLn "roundtrip_type"
  QC.quickCheck prop_roundtrip_type
  --   putStrLn "roundtrip_exp"
  --   QC.quickCheck prop_roundtrip_exp
  putStrLn "\n**typechecker qc tests**\n"
  typecheckerAllQC
  putStrLn "\n**unit tests**\n"
  _ <- test_parser_unit_all
  _ <- test_checker_unit_all
  return ()