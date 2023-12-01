module FlowParser where

import Data.Char qualified as Char
import Data.Map qualified as Map
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

stringValP :: Parser Value
stringValP =
  StringVal
    <$> wsP
      ( P.string "\""
          *> P.many
            (P.satisfy (/= '"'))
          <* P.string "\""
      )

-- TODO: debug this (? definition of Object)
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

-- | Parsing Expressions
expP :: Parser Expression -- type errors will be detected here
expP = compP
  where
    -- compP = catP `P.chainl1` opAtLevel (level Gt)
    -- catP = sumP `P.chainl1` opAtLevel (level Concat)
    -- sumP = prodP `P.chainl1` opAtLevel (level Plus)
    -- prodP = uopexpP `P.chainl1` opAtLevel (level Times)
    compP =
      baseP
        P.<|> Op1 <$> uopP <*> compP
    baseP =
      Val <$> valueP
        P.<|> parens expP
        P.<|> Var <$> varP
        -- P.<|> callP

-- | Parse an operator at a specified precedence level
-- opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
-- opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

-- >>>  P.parse (P.many varP) "x y z"
-- Right [Name "x",Name "y",Name "z"]

-- >>> P.parse varP "(x.y[1]).z"
-- Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) (Val (NumberVal 1)))) "z")
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
        P.<|> flip Proj <$> brackets expP

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
-- >>> P.parse (P.many primitivetypeP) "boolean \n string number null undefined empty any void"
-- Right [BoolType,StringType,NumberType,NullType,UndefinedType,EmptyType,AnyType,VoidType]
primitivetypeP :: Parser PrimitiveType
primitivetypeP =
  constP "boolean" BoolType
    P.<|> constP "string" StringType
    P.<|> constP "number" NumberType
    P.<|> constP "null" NullType
    P.<|> constP "undefined" UndefinedType
    P.<|> constP "empty" EmptyType
    P.<|> constP "any" AnyType
    -- TODO: add object primitive type
    P.<|> constP "void" VoidType

-- | Parser for types
-- >>> P.parse (P.many typeP) "boolean | string ?number"
-- Right [UnionType [BoolType,StringType],MaybeType NumberType]
typeP :: Parser Type
typeP =
  maybetypeP
    P.<|> uniontypeP
    P.<|> PrimitiveType <$> primitivetypeP

-- >>> P.parse (P.many uniontypeP) "boolean | string | number"
-- Right [UnionType [BoolType,StringType,NumberType]]

uniontypeP :: Parser Type
uniontypeP = UnionType <$> P.sepBy1 primitivetypeP (wsP (P.char '|'))

-- >>> P.parse (P.many maybetypeP) "?boolean ?string ?number"
-- Right [MaybeType BoolType,MaybeType StringType,MaybeType NumberType]

maybetypeP :: Parser Type
maybetypeP = MaybeType <$> (P.char '?' *> primitivetypeP)

-- >>> P.parse (P.many callP) "f() f(1) f(1, 2) f(1, 2, 3)"

-- | Parser for function calls
callP :: Parser Expression
callP = undefined

-- | Parser for statements
assignP :: Parser Statement
assignP = Assign <$>
  ((stringP "const" P.<|> stringP "var") *> varP)
  <* wsP (P.char '=') <*> expP

-- >>> P.parse assignP "const x = 1;"
-- Right (Assign (Name "x") (Val (NumberVal 1)))

-- >>> P.parse assignP "var x = \"str\";"
-- Right (Assign (Name "x") (Val (StringVal "str")))

ifP :: Parser Statement
ifP = If <$> (stringP "if" *> parens expP) <*> braces blockP <*> (stringP "else" *> braces blockP)

-- >>> P.parse ifP "if (x > 0) {x = 1} else {x = 2}"
-- Left "No parses"
emptyP = Empty <$ stringP ";"

-- Parses blocks separated by semicolons
blockP :: Parser Block
blockP = Block <$> P.sepBy statementP (stringP ";")

-- >>> P.parse blockP "const x = True; const y = False; y = -x"
-- Right (Block [Assign (Name "x") (Var (Name "True")),Assign (Name "y") (Var (Name "False"))])
