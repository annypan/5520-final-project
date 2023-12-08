module FlowParser where

import Data.Char qualified as Char
import Data.Map qualified as Map
import GHC.Conc qualified as P
import Parser (Parser)
import Parser qualified as P
import Syntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

---------------------- helper functions ----------------------
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

---------------------- Parsing ----------------------

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

-- >>> P.parse (P.many stringValP) "\"str\" \"str\" \n \"str\""
-- Right [StringVal "str",StringVal "str",StringVal "str"]
stringValP :: Parser Value
stringValP =
  StringVal
    <$> wsP
      ( P.string "\""
          *> P.many
            (P.satisfy (/= '"'))
          <* P.string "\""
      )

-- >>> P.parse (P.many objectValP) "{x: 1, y: 2} {z : true, y: \"str\", m: null}"
-- Right [ObjectVal (fromList [("x",NumberVal 1),("y",NumberVal 2)]),ObjectVal (fromList [("m",NullVal),("y",StringVal "str"),("z",BoolVal True)])]
objectValP :: Parser Value
objectValP = ObjectVal <$> wsP (braces (Map.fromList <$> pairsP))

-- >>> P.parse (P.many pairP) "x: 1 y: true z: undefined \n x: null"
-- Right [("x",NumberVal 1),("y",BoolVal True),("z",UndefinedVal),("x",NullVal)]
pairP :: Parser (Name, Value)
pairP = wsP ((,) <$> nameP <* wsP (P.char ':') <*> valueP)

-- >>> P.parse pairsP "x: 1, y: null, z: undefined"
-- Right [("x",NumberVal 1),("y",NullVal),("z",UndefinedVal)]
pairsP :: Parser [(Name, Value)]
pairsP = wsP (P.sepBy pairP (wsP (P.char ',')))

-- | Parsing Expressions
-- >>> P.parse (P.many expP) "f(g(x, y + z * 2))"
-- Right [Call "f" [Call "g" [Var (Name "x"),Op2 (Var (Name "y")) Plus (Op2 (Var (Name "z")) Times (Val (NumberVal 2)))]]]
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
      callP
        P.<|> Var <$> varP
        P.<|> parens expP
        P.<|> Val <$> valueP

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

-- >>> P.parse (P.many callP) "f() f(1) h(1, 2) f(1, 2, 3) g(true, 2, \"str\")"
-- Right [Call "f" [],Call "f" [Val (NumberVal 1)],Call "h" [Val (NumberVal 1),Val (NumberVal 2)],Call "f" [Val (NumberVal 1),Val (NumberVal 2),Val (NumberVal 3)],Call "g" [Val (BoolVal True),Val (NumberVal 2),Val (StringVal "str")]]
callP :: Parser Expression
callP = Call <$> funcNameP <*> parens (P.sepBy expP (wsP (P.char ',')))

-- | Parsing Variables

-- >>> P.parse varP "(x.y[s]).z"
-- Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) "s")) "z")
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

-- >>> P.parse (P.many nameP) "x sfds 23x null, while"
-- Right ["x","sfds"]
nameP :: Parser Name
nameP = P.filter pred (wsP $ (:) <$> beginC <*> P.many c)
  where
    beginC = P.lower P.<|> P.upper P.<|> P.char '_'
    c = beginC P.<|> P.digit
    pred name = name `notElem` reserved

-- >>> P.parse (P.many funcNameP) "f f1 f2 g 1"
-- Right ["f","f1","f2","g"]
funcNameP :: Parser FuncName
funcNameP = P.filter pred (wsP $ (:) <$> beginC <*> P.many c)
  where
    beginC = P.lower P.<|> P.upper P.<|> P.char '_'
    c = beginC P.<|> P.digit
    pred name = name `notElem` reserved

-- | Parsing unary operators
-- >>> P.parse (P.many uopP) "- - ! - typeof -"
-- Right [Neg,Neg,Not,Neg,TypeOf,Neg]
uopP :: Parser Uop
uopP =
  constP "-" Neg
    P.<|> constP "!" Not
    P.<|> constP "typeof" TypeOf

-- | Parsing binary operators
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

-- | Parsing Types
-- >>> P.parse (P.many typeP) "string boolean | string ?number {x: number, y: number}"
-- Right [StringType,UnionType [BoolType,StringType],MaybeType NumberType,ObjectType (fromList [("x",NumberType),("y",NumberType)])]
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

-- >>> P.parse (P.many primitivetypeP) "boolean \n string number null void empty any"
-- Right [BoolType,StringType,NumberType,NullType,UndefinedType,EmptyType,AnyType]
primitivetypeP :: Parser Type
primitivetypeP =
  wsP
    ( constP "boolean" BoolType
        P.<|> constP "string" StringType
        P.<|> constP "number" NumberType
        P.<|> constP "null" NullType
        P.<|> constP "void" UndefinedType
        P.<|> constP "empty" EmptyType
        P.<|> constP "any" AnyType
    )

-- >>> P.parse (P.many pairTypeP) "x: number y: ?boolean z: string | number"
-- Right [("x",NumberType),("y",MaybeType BoolType),("z",UnionType [StringType,NumberType])]
pairTypeP :: Parser (Name, Type)
pairTypeP = wsP ((,) <$> nameP <* wsP (P.char ':') <*> typeP)

-- >>> P.parse pairsTypeP "x: number, y: ?boolean, z: string | number"
-- Right [("x",NumberType),("y",MaybeType BoolType),("z",UnionType [StringType,NumberType])]
pairsTypeP :: Parser [(Name, Type)]
pairsTypeP = wsP (P.sepBy pairTypeP (wsP (P.char ',')))

-- >>> P.parse (P.many objectTypeP) "{x: number, y: boolean} {x: number, y: ?boolean, z: string | number}"
-- Right [ObjectType (fromList [("x",NumberType),("y",BoolType)]),ObjectType (fromList [("x",NumberType),("y",MaybeType BoolType),("z",UnionType [StringType,NumberType])])]
objectTypeP :: Parser Type
objectTypeP = wsP (ObjectType <$> braces (Map.fromList <$> pairsTypeP))

-- >>> P.parse (P.many uniontypeP) "boolean | string | ?number"
-- Right [UnionType [BoolType,StringType,MaybeType NumberType]]
uniontypeP :: Parser Type
uniontypeP = wsP (UnionType <$> P.sepBy1 (maybetypeP P.<|> primitivetypeP) (wsP (P.char '|')))

-- >>> P.parse (P.many maybetypeP) "?boolean ?string ?number"
-- Right [MaybeType BoolType,MaybeType StringType,MaybeType NumberType]
maybetypeP :: Parser Type
maybetypeP = wsP (MaybeType <$> (P.char '?' *> primitivetypeP))

-- >>> P.parse (P.many fpairTypeP) "x: number y: string z: any"
-- Right [("x",NumberType),("y",StringType),("z",AnyType)]
fpairTypeP :: Parser (Name, Type)
fpairTypeP = wsP ((,) <$> nameP <* wsP (P.char ':') <*> typeP)

-- >>> P.parse fpairsTypeP "(x: boolean, y: string)"
-- Right [BoolType,StringType]
fpairsTypeP :: Parser [(Name, Type)]
fpairsTypeP = parens (P.sepBy pairTypeP (wsP (P.char ',')))

-- >>> P.parse (P.many functiontypeP) "(x: boolean, y: string): number"
-- Right [FunctionType [("x",BoolType),("y",StringType)] NumberType]
functiontypeP :: Parser Type
functiontypeP = wsP (FunctionType <$> fpairsTypeP <*> (wsP (P.string ":") *> typeP))

-- | Parsing Statements

-- >>> P.parse (P.many statementP) "const x = true; var y = 2; if (x > 0) { x = 1; } else { x = 2; }"
-- Right [Assign (Name "x") (Val (BoolVal True)),Empty,Assign (Name "y") (Val (NumberVal 2)),Empty,If (Op2 (Var (Name "x")) Gt (Val (NumberVal 0))) (Block [Update (Name "x") (Val (NumberVal 1))]) (Block [Update (Name "x") (Val (NumberVal 2))])]
statementP :: Parser Statement
statementP =
  emptyP
    P.<|> assignP
    P.<|> updateP
    P.<|> ifP
    P.<|> whileP
    P.<|> forP
    P.<|> returnP
    P.<|> functionDefP
    P.<|> functionCallP

-- >>> P.parse assignP "let x = \"str\";"
-- Right (Assign (Name "x") (Val (StringVal "str")))
assignP :: Parser Statement
assignP =
  Assign
    <$> ((stringP "const" P.<|> stringP "var" P.<|> stringP "let") *> varP)
    <* wsP (P.char '=')
    <*> expP

-- >>> P.parse updateP "x = x + 1;"
-- Right (Update (Name "x") (Op2 (Var (Name "x")) Plus (Val (NumberVal 1))))
updateP :: Parser Statement
updateP = Update <$> varP <* wsP (P.char '=') <*> expP

-- >>> P.parse ifP "if (x > 0) { x = 1; y = 2;} else { x = 2; }"
-- Right (If (Op2 (Var (Name "x")) Gt (Val (NumberVal 0))) (Block [Update (Name "x") (Val (NumberVal 1)),Update (Name "y") (Val (NumberVal 2))]) (Block [Update (Name "x") (Val (NumberVal 2))]))
ifP :: Parser Statement
ifP = If <$> (stringP "if" *> parens expP) <*> braces blockP <*> (stringP "else" *> braces blockP)

emptyP :: Parser Statement
emptyP = Empty <$ stringP ";"

-- >>> P.parse whileP "while (x > 0) {x = x - 1;}"
-- Right (While (Op2 (Var (Name "x")) Gt (Val (NumberVal 0))) (Block [Update (Name "x") (Op2 (Var (Name "x")) Minus (Val (NumberVal 1)))]))
whileP :: Parser Statement
whileP = While <$> (stringP "while" *> parens expP) <*> braces blockP

-- >>> P.parse forP "for (let i = 0; i < 10; i = i + 1) {x = x + 1;}"
-- Right (For (Assign (Name "i") (Val (NumberVal 0))) (Op2 (Var (Name "i")) Lt (Val (NumberVal 10))) (Update (Name "i") (Op2 (Var (Name "i")) Plus (Val (NumberVal 1)))) (Block [Update (Name "x") (Op2 (Var (Name "x")) Plus (Val (NumberVal 1)))]))
forP :: Parser Statement
forP = For <$> (stringP "for" *> stringP "(" *> statementP) <* stringP ";" <*> expP <* stringP ";" <*> statementP <* stringP ")" <*> braces blockP

-- >>> P.parse returnP "return x+y;"
-- Right (Return (Op2 (Var (Name "x")) Plus (Var (Name "y"))))
returnP :: Parser Statement
returnP = Return <$> (stringP "return" *> expP)

-- >>> P.parse functionDefP "function f(x: number, y: number): number { return x + y; }"
-- Right (FunctionDef "f" (FunctionType [("x",NumberType),("y",NumberType)] NumberType) (Block [Return (Op2 (Var (Name "x")) Plus (Var (Name "y")))]))
functionDefP :: Parser Statement
functionDefP =
  FunctionDef
    <$> (stringP "function" *> funcNameP)
    <*> functiontypeP
    <*> braces blockP

-- >>> P.parse functionCallP "f(1, false)"
-- Right (FunctionCall "f" [Val (NumberVal 1),Val (BoolVal False)])
functionCallP :: Parser Statement
functionCallP = FunctionCall <$> funcNameP <*> parens (P.sepBy expP (wsP (P.char ',')))

-- | Parsing blocks separated by semicolons

-- >>> P.parse blockP "const x = true; var y = 2; if (x > 0) { x = 1; } else { x = 2; }"
-- Right (Block [Assign (Name "x") (Val (BoolVal True)),Assign (Name "y") (Val (NumberVal 2)),If (Op2 (Var (Name "x")) Gt (Val (NumberVal 0))) (Block [Update (Name "x") (Val (NumberVal 1))]) (Block [Update (Name "x") (Val (NumberVal 2))])])
blockP :: Parser Block
blockP = Block <$> (P.many statementP >>= \s -> return (filter (/= Empty) s))

parseJSFile :: String -> IO (Either P.ParseError Block)
parseJSFile = P.parseFromFile blockP