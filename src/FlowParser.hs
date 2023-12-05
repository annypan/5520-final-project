module FlowParser where

import Data.Char qualified as Char
import Data.Map qualified as Map
import GHC.Conc qualified as P
import Parser (Parser)
import Parser qualified as P
import Syntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

--------------------- helper functions ---------------------
wsP :: Parser a -> Parser a
wsP p = p <* P.many (P.satisfy Char.isSpace)

stringP :: String -> Parser ()
stringP s = wsP (P.string s) *> pure ()

constP :: String -> a -> Parser a
constP s x = wsP (P.string s) *> pure x

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

---------------------- Parsing ---------------------

-- | Parsing Statements
statementP :: Parser Statement
statementP = assignP

-- | Parsing Constants
valueP :: Parser Value
valueP =
  boolValP
    P.<|> stringValP
    P.<|> numberValP
    P.<|> objectValP
    P.<|> undefinedValP
    P.<|> nullValP

-- >>> P.parse (P.many valueP) "true false null undefined 12 \"str\""
-- Right [BoolVal True,BoolVal False,NullVal,UndefinedVal,NumberVal 12,StringVal "str"]

-- >>> P.parse (P.many boolValP) "true false\n true"
-- Right [BoolVal True,BoolVal False,BoolVal True]
boolValP :: Parser Value
boolValP = constP "true" (BoolVal True) P.<|> constP "false" (BoolVal False)

-- >>> P.parse (P.many undefinedValP) "undefined \n undefined"
-- Right [UndefinedVal,UndefinedVal]
undefinedValP :: Parser Value
undefinedValP = constP "undefined" UndefinedVal

-- >>> P.parse (P.many nullValP) "null null \n null"
-- Right [NullVal,NullVal,NullVal]
nullValP :: Parser Value
nullValP = constP "null" NullVal

-- >>> P.parse (P.many numberValP) "12 3 \n 4"
-- Right [NumberVal 12,NumberVal 3,NumberVal 4]
numberValP :: Parser Value
numberValP = NumberVal <$> wsP P.int

-- >>> P.parse (P.many stringValP) "\"str\" \"str\""
-- Right [StringVal "str",StringVal "str"]
stringValP :: Parser Value
stringValP =
  StringVal
    <$> wsP
      ( P.string "\""
          *> P.many
            (P.satisfy (/= '"'))
          <* P.string "\""
      )

objectValP :: Parser Value
objectValP = ObjectVal <$> wsP (braces (Map.fromList <$> pairsP))

-- >>> P.parse (objectValP) "{x: 1, y: 2}"
-- Right (ObjectVal (fromList [("x",NumberVal 1),("y",NumberVal 2)]))

-- >>> P.parse pairP "x: 1"
-- Right ("x",NumberVal 1)
pairP :: Parser (Name, Value)
pairP = wsP ((,) <$> nameP <* wsP (P.char ':') <*> valueP)

-- >>> P.parse pairsP "x: 1, y: 2"
-- Right [("x",NumberVal 1),("y",NumberVal 2)]
-- >>> P.parse pairsP "x: 1, y: null, z: undefined"
-- Right [("x",NumberVal 1),("y",NullVal),("z",UndefinedVal)]
pairsP :: Parser [(Name, Value)]
pairsP = wsP (P.sepBy pairP (wsP (P.char ',')))

-- >>> P.parse (P.many expP) "x > 1"
-- Right [Op2 (Var (Name "x")) Gt (Val (NumberVal 1))]

-- | Parsing Expressions
expP :: Parser Expression -- type errors will be detected here
expP = compP
  where
    compP = catP `P.chainl1` opAtLevel (level Gt)
    catP = sumP `P.chainl1` opAtLevel (level Concat)
    sumP = prodP `P.chainl1` opAtLevel (level Plus)
    prodP = uopexpP `P.chainl1` opAtLevel (level Times)
    uopexpP =
      baseP
        P.<|> Op1 <$> uopP <*> compP
    baseP =
      Var <$> varP
        P.<|> parens expP
        P.<|> Val <$> valueP
        P.<|> callP

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

-- >>>  P.parse (P.many varP) "x y z"
-- Right [Name "x",Name "y",Name "z"]

-- >>> P.parse varP "(x.y[s]).z"
-- Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) "s")) "z")
-- >>> P.parse varP "x.y.z"
-- Right (Dot (Var (Dot (Var (Name "x")) "y")) "z")
varP :: Parser Var
varP = mkVar <$> prefixP <*> P.some indexP P.<|> Name <$> nameP
  where
    mkVar :: Expression -> [Expression -> Var] -> Var
    mkVar e l = foldr1 (\f p u -> p (Var (f u))) l e

    prefixP :: Parser Expression
    prefixP = parens expP P.<|> Var . Name <$> nameP

    indexP :: Parser (Expression -> Var)
    indexP =
      flip Dot <$> (P.string "." *> nameP)
        P.<|> flip Proj <$> brackets nameP

reserved :: [String]
reserved =
  [ "and",
    "await",
    "break",
    "case",
    "catch",
    "class",
    "const",
    "continue",
    "default",
    "delete",
    "do",
    "else",
    "elseif",
    "end",
    "enum",
    "false",
    "for",
    "function",
    "goto",
    "if",
    "in",
    "instanceof",
    "not",
    "null",
    "new",
    "or",
    "repeat",
    "return",
    "then",
    "this",
    "throw",
    "true",
    "try",
    "typeof",
    "undefined",
    "until",
    "var",
    "void",
    "while"
  ]

-- >>> P.parse (P.many nameP) "x sfds null 23x x3"
-- Right ["x","sfds"]

-- TODO: fix this
-- current stops interpretation after the first invalid name

nameP :: Parser Name
nameP = P.filter pred (wsP $ (:) <$> beginC <*> P.many c)
  where
    beginC = P.lower P.<|> P.upper P.<|> P.char '_'
    c = beginC P.<|> P.digit
    pred name = name `notElem` reserved

-- >>> P.parse (P.many funcNameP) "f f1 f2"
-- Right ["f","f1","f2"]

funcNameP :: Parser FuncName
funcNameP = P.filter pred (wsP $ (:) <$> beginC <*> P.many c)
  where
    beginC = P.lower P.<|> P.upper P.<|> P.char '_'
    c = beginC P.<|> P.digit
    pred name = name `notElem` reserved

-- | Parser for unary operators
-- >>> P.parse (P.many uopP) "- - ! - typeof -"
-- Right [Neg,Neg,Not,Neg,TypeOf,Neg]
uopP :: Parser Uop
uopP =
  constP "-" Neg
    P.<|> constP "!" Not
    P.<|> constP "typeof" TypeOf

-- | Parser for binary operators
-- >>> P.parse (P.many bopP) "+ - * // % == != > >= < <= .. in"
-- Right [Plus,Minus,Times,Divide,Modulo,Eq,Neq,Gt,Ge,Lt,Le,Concat,In]
bopP :: Parser Bop
bopP =
  wsP
    ( (Plus <$ P.char '+')
        P.<|> (Minus <$ P.char '-')
        P.<|> (Times <$ P.char '*')
        P.<|> (Divide <$ P.string "//")
        P.<|> (Modulo <$ P.char '%')
        P.<|> (Eq <$ P.string "==")
        P.<|> (Neq <$ P.string "!=")
        P.<|> (Ge <$ P.string ">=")
        P.<|> (Le <$ P.string "<=")
        P.<|> (Lt <$ P.char '<')
        P.<|> (Gt <$ P.char '>')
        P.<|> (Concat <$ P.string "..")
        P.<|> (In <$ P.string "in")
    )

-- | Parser for primitive types
-- >>> P.parse (P.many primitivetypeP) "boolean \n string number null undefined empty any"
-- Right [BoolType,StringType,NumberType,NullType,UndefinedType,EmptyType,AnyType]
primitivetypeP :: Parser Type
primitivetypeP =
  wsP
    ( constP "boolean" BoolType
        P.<|> constP "string" StringType
        P.<|> constP "number" NumberType
        P.<|> constP "null" NullType
        P.<|> constP "undefined" UndefinedType
        P.<|> constP "empty" EmptyType
        P.<|> constP "any" AnyType
    )

pairTypeP :: Parser (Name, Type)
pairTypeP = wsP ((,) <$> nameP <* wsP (P.char ':') <*> typeP)

pairsTypeP :: Parser [(Name, Type)]
pairsTypeP = wsP (P.sepBy pairTypeP (wsP (P.char ',')))

objectTypeP :: Parser Type
objectTypeP = wsP (ObjectType <$> braces (Map.fromList <$> pairsTypeP))

-- | Parser for types
-- >>> P.parse (P.many typeP) "string boolean | string ?number {x: number, y: number}"
-- Right [StringType,UnionType [BoolType,StringType],MaybeType NumberType,ObjectType (fromList [("x",NumberType),("y",NumberType)])]

-- >>> P.parse (typeP) "number"
-- Right NumberType

-- >>> P.parse (P.many typeP) "(string, boolean)  : number boolean | string | number"
-- Right [FunctionType [StringType,BoolType] NumberType,UnionType [BoolType,StringType,NumberType]]

typeP :: Parser Type
typeP =
  maybetypeP
    P.<|> uniontypeP
    P.<|> primitivetypeP
    P.<|> functiontypeP
    P.<|> objectTypeP
    >>= \t ->
      case t of
        UnionType [m] -> return m -- remove redundant union
        _ -> return t

-- >>> P.parse (P.many uniontypeP) "boolean | string | number"
-- Right [UnionType [BoolType,StringType,NumberType]]

uniontypeP :: Parser Type
uniontypeP = wsP (UnionType <$> P.sepBy1 primitivetypeP (wsP (P.char '|')))

-- >>> P.parse (P.many maybetypeP) "?boolean ?string ?number"
-- Right [MaybeType BoolType,MaybeType StringType,MaybeType NumberType]

maybetypeP :: Parser Type
maybetypeP = wsP (MaybeType <$> (P.char '?' *> primitivetypeP))

-- >>> P.parse (P.many functiontypeP) "(boolean, string): number"
-- Right [FunctionType [BoolType,StringType] NumberType]
functiontypeP :: Parser Type
functiontypeP = wsP (FunctionType <$> parens (P.sepBy typeP (wsP (P.char ','))) <*> (wsP (P.string ":") *> typeP))

-- >>> P.parse (P.many callP) "f() f(1) f(1, 2) f(1, 2, 3)"
-- Right [Call "f" [],Call "f" [Val (NumberVal 1)],Call "f" [Val (NumberVal 1),Val (NumberVal 2)],Call "f" [Val (NumberVal 1),Val (NumberVal 2),Val (NumberVal 3)]]

-- | Parser for function calls

-- >>> P.parse (P.many callP) "f()  f(1) f(1, 2)"
-- Right [Call "f" [],Call "f" [Val (NumberVal 1)],Call "f" [Val (NumberVal 1),Val (NumberVal 2)]]

-- >>>  P.parse (P.many callP) "f(true, 2, \"str\")"
-- Right [Call "f" [Val (BoolVal True),Val (NumberVal 2),Val (StringVal "str")]]
callP :: Parser Expression
callP = Call <$> funcNameP <*> parens (P.sepBy expP (wsP (P.char ',')))

-- | Parser for statements
assignP :: Parser Statement
assignP =
  Assign
    <$> ((stringP "const" P.<|> stringP "var") *> varP)
    <* wsP (P.char '=')
    <*> expP

-- >>> P.parse assignP "const x = 1;"
-- Right (Assign (Name "x") (Val (NumberVal 1)))

-- >>> P.parse assignP "const x = false;"
-- Right (Assign (Name "x") (Val (BoolVal False)))

-- >>> P.parse assignP "var x = \"str\";"
-- Right (Assign (Name "x") (Val (StringVal "str")))

ifP :: Parser Statement
ifP = If <$> (stringP "if" *> parens expP) <*> braces blockP <*> (stringP "else" *> braces blockP)

-- >>> P.parse blockP "const x = 1"
-- Right (Block [Assign (Name "x") (Val (NumberVal 1))])

-- >>> P.parse expP "x > 1"
-- Right (Var (Name "x"))

-- ifP = do
--   stringP "if"
--   cond <- parens expP
--   thenBlock <- braces blockP
--   stringP "else"
--   elseBlock <- braces blockP
--   return $ If cond thenBlock elseBlock

-- >>> P.parse ifP "if (x > 0) {const x = 1} else {const x = 2}"
-- Left "No parses"
emptyP = Empty <$ stringP ";"

whileP :: Parser Statement
whileP = While <$> (stringP "while" *> parens expP) <*> braces blockP

-- >>> P.parse whileP "while (x > 0) {x = x - 1}"
-- Left "No parses"

-- Parses blocks separated by semicolons
blockP :: Parser Block
blockP = Block <$> P.sepBy statementP (wsP (stringP ";"))

-- >>> P.parse blockP "const x = true; const y = false; y = -x"
-- Right (Block [Assign (Name "x") (Val (BoolVal True)),Assign (Name "y") (Val (BoolVal False))])

parseJSFile :: String -> IO (Either P.ParseError Block)
parseJSFile = P.parseFromFile blockP

tParseFiles :: Test
tParseFiles =
  "parse files" ~:
    TestList
      [ "assign" ~: p "js/assign.js" wAssign,
        "assignConflict" ~: p "js/assignConflict.js" wAssignConflict
      ]
  where
    p fn ast = do
      result <- parseJSFile fn
      case result of
        Left _ -> assert False
        Right ast' -> assert (ast == ast')
