module Syntax where

import Data.Char qualified as Char
import Data.Map
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck qualified as QC

type Object = Map Name Value

type Name = String

type FuncName = String

data Statement
  = Assign Var Expression -- const x = e; var x = e -- handles var, const
  | Update Var Expression -- x = e
  | If Expression Block Block -- if e then s1 else s2 end
  | While Expression Block -- while e do s end
  | Empty -- ';'
  | For Statement Expression Expression Block -- for (s1; e1; e2) {s2}
  | Return Expression -- return e
  | FunctionDef FuncName Type Block -- function f(x1, ..., xn) s -- should actually be functype
  deriving (Eq, Show)

data Var
  = Name Name -- x, global variable
  | Dot Expression Name -- t.x, access the object property x
  | Proj Expression Name -- t[1], access the object property 1
  deriving (Eq, Show)

newtype Block = Block [Statement] -- s1 ... sn
  deriving (Eq, Show)

instance Semigroup Block where
  Block s1 <> Block s2 = Block (s1 <> s2)

instance Monoid Block where
  mempty = Block []

data Expression
  = Val Value -- literal values
  | Var Var -- variables
  | Op1 Uop Expression -- unary operators
  | Op2 Expression Bop Expression -- binary operators
  | Call FuncName [Expression] -- function calls
  deriving (Eq, Show)

data Value
  = BoolVal Bool -- https://flow.org/en/docs/types/literals/
  | StringVal String
  | NumberVal Int
  | ObjectVal Object -- https://flow.org/en/docs/types/objects/
  | UndefinedVal
  | NullVal
  deriving (Eq, Show, Ord)

data Type
  = BoolType
  | StringType
  | NumberType
  | NullType
  | UndefinedType
  | EmptyType -- https://flow.org/en/docs/types/empty/
  | AnyType -- https://flow.org/en/docs/types/any/
  | UnionType [Type]
  | MaybeType Type
  | FunctionType [Type] Type
  | ObjectType (Map String Type)
  deriving (Eq, Show, Ord)

data Uop
  = Neg
  | Not
  | TypeOf -- https://www.digitalocean.com/community/tutorials/javascript-unary-operators-simple-and-useful#typeof
  deriving (Eq, Show, Enum, Bounded)

data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Modulo
  | Eq
  | Neq
  | Gt
  | Ge
  | Lt
  | Le
  | Concat
  | In -- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/in
  deriving (Eq, Show, Enum, Bounded)

level :: Bop -> Int
level Times = 7
level Divide = 7
level Plus = 5
level Minus = 5
level Concat = 4
level _ = 3 -- comparison operators

-- | Generate a string literal, being careful about the characters that it may contain
genStringLit :: Gen String
genStringLit = escape <$> QC.listOf (QC.elements stringLitChars)
  where
    -- escape special characters appearing in the string,
    escape :: String -> String
    escape = Prelude.foldr Char.showLitChar ""
    -- generate strings containing printable characters or spaces, but not including '\"'
    stringLitChars :: [Char]
    stringLitChars = Prelude.filter (\c -> c /= '\"' && (Char.isSpace c || Char.isPrint c)) ['\NUL' .. '~']

shrinkStringLit :: String -> [String]
shrinkStringLit s = Prelude.filter (/= '\"') <$> shrink s

instance Arbitrary Value where
  arbitrary =
    QC.oneof
      [ BoolVal <$> arbitrary,
        StringVal <$> genStringLit,
        NumberVal <$> arbitrary,
        -- , ObjectVal <$> arbitrary -- TODO: add later
        pure UndefinedVal,
        pure NullVal
      ]

  shrink (BoolVal b) = BoolVal <$> shrink b
  shrink (StringVal s) = StringVal <$> shrinkStringLit s
  shrink (NumberVal n) = NumberVal <$> shrink n
  -- shrink (ObjectVal o) = ObjectVal <$> shrink o
  shrink _ = []

genUnionTypeCandidate :: Gen Type
genUnionTypeCandidate = QC.elements [BoolType, StringType, NumberType, NullType, UndefinedType, EmptyType, AnyType]

genMaybeTypeCandidate :: Gen Type
genMaybeTypeCandidate = QC.elements [BoolType, StringType, NumberType, NullType, UndefinedType, EmptyType, AnyType]

genMap :: Gen (Map String Type)
genMap = QC.listOf1 ((,) <$> genName <*> genUnionTypeCandidate) >>= \ts -> pure (fromList ts)

instance Arbitrary Type where
  arbitrary =
    QC.oneof
      [ pure BoolType,
        pure StringType,
        pure NumberType,
        pure NullType,
        pure UndefinedType,
        pure EmptyType,
        pure AnyType,
        UnionType <$> ((++) <$> QC.listOf1 genUnionTypeCandidate <*> QC.listOf1 genUnionTypeCandidate),
        MaybeType <$> genMaybeTypeCandidate,
        FunctionType <$> QC.listOf1 genMaybeTypeCandidate <*> genMaybeTypeCandidate,
        ObjectType <$> genMap
      ]
  shrink BoolType = []
  shrink StringType = []
  shrink NumberType = []
  shrink NullType = []
  shrink UndefinedType = []
  shrink EmptyType = []
  shrink AnyType = []
  shrink (UnionType ts) = [UnionType ts' | ts' <- shrink ts, length ts' > 1]
  shrink (MaybeType t) = [t, UndefinedType, NullType]
  shrink (FunctionType args ret) = [FunctionType args' ret | args' <- shrink args, not (Prelude.null args')]
  shrink (ObjectType map) = [ObjectType map' | map' <- shrink map, "" `notElem` keys map']

instance Arbitrary Uop where
  arbitrary = QC.elements [minBound .. maxBound]
  shrink = const []

instance Arbitrary Bop where
  arbitrary = QC.elements [minBound .. maxBound]
  shrink = const []

genExp :: Int -> Gen Expression
genExp 0 = QC.oneof [Var <$> genVar 0, Val <$> arbitrary]
genExp n =
  QC.frequency
    [ (1, Var <$> genVar n),
      (1, Val <$> arbitrary),
      (n, Op1 <$> arbitrary <*> genExp n'),
      (n, Op2 <$> genExp n' <*> arbitrary <*> genExp n'),
      (n, Call <$> genName <*> QC.listOf (genExp n'))
    ]
  where
    n' = n `div` 2

genName :: Gen Name
genName = QC.elements ["_", "_G", "x", "X", "y", "x0", "X0", "xy", "XY", "_x"]

genVar :: Int -> Gen Var
genVar 0 = Name <$> genName
genVar n =
  QC.frequency
    [ (1, Name <$> genName),
      (n, Dot <$> genExp n' <*> genName),
      (n, Proj <$> genExp n' <*> genName)
    ]
  where
    n' = n `div` 2

instance Arbitrary Var where
  arbitrary = QC.sized genVar
  shrink (Name n) = []
  shrink (Dot e n) = [Dot e' n | e' <- shrink e]
  shrink (Proj e n) = [Proj e' n | e' <- shrink e]

instance Arbitrary Expression where
  arbitrary =
    QC.oneof
      [ Val <$> arbitrary,
        Var <$> arbitrary,
        Op1 <$> arbitrary <*> arbitrary,
        Op2 <$> arbitrary <*> arbitrary <*> arbitrary,
        Call <$> arbitrary <*> arbitrary
      ]
  shrink (Val v) = Val <$> shrink v
  shrink (Var v) = Var <$> shrink v
  shrink (Op1 uop e) = Op1 uop <$> shrink e
  shrink (Op2 e1 bop e2) = Op2 <$> shrink e1 <*> pure bop <*> shrink e2
  shrink (Call fn es) = Call <$> shrink fn <*> shrink es

-- assign.js
wAssign :: Block
wAssign = Block [Assign (Name "x") (Val (BoolVal True)), Assign (Name "y") (Val (BoolVal False))]

-- assignConflict.js
wAssignConflict :: Block
wAssignConflict =
  Block
    [ Assign (Name "x") (Val (BoolVal True)),
      Assign (Name "y") (Val (BoolVal False)),
      Assign (Name "x") (Val (NumberVal 1))
    ]

wIf :: Block
wIf =
  Block
    [ Assign (Name "x") (Val (BoolVal True)),
      Assign (Name "y") (Val (NumberVal 2)),
      If
        (Op2 (Var (Name "x")) Eq (Val (BoolVal True)))
        (Block [Update (Name "y") (Val (NumberVal 3))])
        (Block [Update (Name "y") (Val (NumberVal 4))])
    ]
