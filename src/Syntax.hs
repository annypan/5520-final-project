module Syntax where

import Data.Map
import Test.QuickCheck (Arbitrary (..), Gen)
import qualified Test.QuickCheck as QC
import qualified Data.Char as Char

type Object = Map Name Value
type Name = String
type FuncName = String

data Statement
  = Assign Var Expression -- x = e -- handles var, let, const
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
      [ BoolVal <$> arbitrary
      , StringVal <$> genStringLit
      , NumberVal <$> arbitrary
      -- , ObjectVal <$> arbitrary -- TODO: add later
      , pure UndefinedVal
      , pure NullVal
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

instance Arbitrary Type where
  arbitrary = 
    QC.oneof
      [ pure BoolType
      , pure StringType
      , pure NumberType
      , pure NullType
      , pure UndefinedType
      , pure EmptyType
      , pure AnyType
      , UnionType <$> (QC.listOf1 genUnionTypeCandidate 
        >>= \t -> if length t == 1 then arbitrary else pure t)
      , MaybeType <$> genMaybeTypeCandidate
      -- , FunctionType <$> arbitrary <*> arbitrary -- TODO: add later
      -- , ObjectType <$> arbitrary -- TODO: add later
      ]
  shrink BoolType = []
  shrink StringType = []
  shrink NumberType = []
  shrink NullType = []
  shrink UndefinedType = []
  shrink EmptyType = []
  shrink AnyType = []
  shrink (UnionType ts) = UnionType <$> shrink ts
  shrink (MaybeType t) = MaybeType <$> shrink t
  shrink (FunctionType args ret) = FunctionType <$> shrink args <*> shrink ret
  shrink (ObjectType map) = ObjectType <$> shrink map

instance Arbitrary Uop where
  arbitrary = QC.elements [minBound .. maxBound]
  shrink = const []

instance Arbitrary Bop where
  arbitrary = QC.elements [minBound .. maxBound]
  shrink = const []

-- genName :: Gen Name
-- genName = QC.elements ["_", "_G", "x", "X", "y", "x0", "X0", "xy", "XY", "_x"]

-- genVar :: Int -> Gen Var
-- genVar 0 = Name <$> genName
-- genVar n =
--   QC.frequency
--     [ (1, Name <$> genName),
--       (n, Dot <$> genExp n' <*> genName),
--       (n, Proj <$> genExp n' <*> genExp n')
--     ]
--   where
--     n' = n `div` 2

-- instance Arbitrary Var where
--   arbitrary = QC.sized genVar
--   shrink (Name n) = []
--   shrink (Dot e n) = [Dot e' n | e' <- shrink e]
--   shrink (Proj e1 e2) =
--     [Proj e1' e2 | e1' <- shrink e1]
--       ++ [Proj e1 e2' | e2' <- shrink e2]

-- instance Arbitrary Expression where
--   arbitrary = 
--     QC.oneof
--       [ Val <$> arbitrary
--       , Var <$> arbitrary
--       , Op1 <$> arbitrary <*> arbitrary
--       , Op2 <$> arbitrary <*> arbitrary <*> arbitrary
--       , Call <$> arbitrary <*> arbitrary
--       ]
--   shrink (Val v) = Val <$> shrink v
--   shrink (Var v) = Var <$> shrink v
--   shrink (Op1 uop e) = Op1 <$> pure uop <*> shrink e
--   shrink (Op2 e1 bop e2) = Op2 <$> shrink e1 <*> pure bop <*> shrink e2
--   shrink (Call fn es) = Call <$> shrink fn <*> shrink es

-- assign.js
wAssign :: Block
wAssign = Block [Assign (Name "x") (Val (BoolVal True)),Assign (Name "y") (Val (BoolVal False))]

-- assignConflict.js
wAssignConflict :: Block
wAssignConflict = 
  Block [
    Assign (Name "x") (Val (BoolVal True)),
    Assign (Name "y") (Val (BoolVal False)),
    Assign (Name "x") (Val (NumberVal 1))]
