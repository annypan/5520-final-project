module Syntax where

import Data.Map
import Test.QuickCheck (Arbitrary (..), Gen)
import qualified Test.QuickCheck as QC

type Object = Map Value Value
type Name = String
type FuncName = String
type FunctionArg = (Name, Type)

data Statement
  = Assign Var Expression -- x = e -- handles var, let, const
  | If Expression Block Block -- if e then s1 else s2 end
  | While Expression Block -- while e do s end
  | Empty -- ';'
  | For Statement Expression Expression Block -- for (s1; e1; e2) {s2}
  | Return Expression -- return e
  | FunctionDef FuncName [FunctionArg] Type Block -- function f(x1, ..., xn) s

data Var
  = Name Name -- x, global variable
  | Dot Expression Name -- t.x, access the object property x
  | Proj Expression Expression -- t[1], access the object property 1
  deriving (Eq, Show)

newtype Block = Block [Statement] -- s1 ... sn

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

data PrimitiveType
  = BoolType
  | StringType
  | NumberType
  | NullType
  | UndefinedType
  | EmptyType -- https://flow.org/en/docs/types/empty/
  | AnyType -- https://flow.org/en/docs/types/any/
  | ObjectType
  | VoidType -- only used for function return types
  deriving (Eq, Show, Ord)

data Type
  = PrimitiveType PrimitiveType
  | UnionType [PrimitiveType]
  | MaybeType PrimitiveType
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

instance Arbitrary Value where
  arbitrary = 
    QC.oneof
      [ BoolVal <$> arbitrary
      , StringVal <$> arbitrary
      , NumberVal <$> arbitrary
      -- , ObjectVal <$> arbitrary
      , pure UndefinedVal
      , pure NullVal
      ]
  
  shrink (BoolVal b) = BoolVal <$> shrink b
  shrink (StringVal s) = StringVal <$> shrink s
  shrink (NumberVal n) = NumberVal <$> shrink n
  -- shrink (ObjectVal o) = ObjectVal <$> shrink o
  shrink _ = []

instance Arbitrary PrimitiveType where
  arbitrary = 
    QC.oneof
      [ pure BoolType
      , pure StringType
      , pure NumberType
      , pure NullType
      , pure UndefinedType
      , pure EmptyType
      , pure AnyType
      , pure ObjectType
      , pure VoidType
      ]
  shrink = const []

instance Arbitrary Type where
  arbitrary = 
    QC.oneof
      [ PrimitiveType <$> arbitrary
      , UnionType <$> arbitrary
      , MaybeType <$> arbitrary
      ]
  shrink (PrimitiveType t) = []
  shrink (UnionType ts) = UnionType <$> shrink ts
  shrink (MaybeType t) = MaybeType <$> shrink t

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