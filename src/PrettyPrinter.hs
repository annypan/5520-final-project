module PrettyPrinter where
import Syntax
import Text.PrettyPrint (Doc, (<+>))
import Text.PrettyPrint qualified as PP
import Data.Map (Map)
import qualified Data.Map as Map

class PP a where
  pp :: a -> Doc

-- | Default operation for the pretty printer. Displays using standard formatting
-- rules, with generous use of indentation and newlines.
pretty :: (PP a) => a -> String
pretty = PP.render . pp

instance PP Uop where
  pp Neg = PP.text "-"
  pp Not = PP.text "not"
  pp TypeOf = PP.text "typeof"

instance PP Bool where
  pp True = PP.text "true"
  pp False = PP.text "false"

instance PP String where
  pp = PP.doubleQuotes . PP.text

instance PP Bop where
  pp Plus = PP.text "+"
  pp Minus = PP.text "-"
  pp Times = PP.text "*"
  pp Divide = PP.text "/"
  pp Modulo = PP.text "%"
  pp Eq = PP.text "=="
  pp Neq = PP.text "!="
  pp Gt = PP.text ">"
  pp Ge = PP.text ">="
  pp Lt = PP.text "<"
  pp Le = PP.text "<="
  pp Concat = PP.text "++"
  pp In = PP.text "in"

instance PP (Map Name Value) where
  pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
    where
      ppa (n, v) = PP.text n <+> PP.text "=" <+> pp v

instance PP Value where
    pp (BoolVal b) = pp b
    pp (StringVal s) = pp s
    pp (NumberVal n) = PP.int n
    pp (ObjectVal o) = pp o
    pp UndefinedVal = PP.text "undefined"
    pp NullVal = PP.text "null"

instance PP a => PP (Map Value a) where
  pp m = PP.braces (PP.vcat (map ppa (Map.toList m)))
    where
      ppa (StringVal s, v2) = PP.text s <+> PP.text "=" <+> pp v2
      ppa (v1, v2) = PP.brackets (pp v1) <+> PP.text "=" <+> pp v2

instance PP Var where
  pp (Name n) = PP.text n
  pp (Dot (Var v) k) = pp v <> PP.text "." <> pp k
  pp (Dot t k) = PP.parens (pp t) <> PP.text "." <> pp k
  pp (Proj (Var v) k) = pp v <> PP.brackets (pp k)
  pp (Proj t k) = PP.parens (pp t) <> PP.brackets (pp k)

instance PP PrimitiveType where
  pp BoolType = PP.text "bool"
  pp StringType = PP.text "string"
  pp NumberType = PP.text "number"
  pp NullType = PP.text "null"
  pp UndefinedType = PP.text "undefined"
  pp EmptyType = PP.text "empty"
  pp AnyType = PP.text "any"
  pp ObjectType = PP.text "object"

instance PP Type where
  pp (PrimitiveType t) = pp t
  pp (UnionType ts) = PP.hsep (PP.punctuate PP.comma (fmap pp ts))
  pp (MaybeType t) = PP.text "?" <> pp t

instance PP Statement where
  pp (Assign var e) = pp var <+> PP.text "=" <+> pp e
  pp (If e s1 s2) =
    PP.text "if" <+> pp e <+> PP.text "then" <+> pp s1 <+> PP.text "else" <+> pp s2
  pp (While e s) = PP.text "while" <+> pp e <+> PP.text "do" <+> pp s
  pp Empty = PP.text ";"
  pp (For s1 e1 e2 s2) =
    PP.text "for" -- TODO: refine
  pp (Return e) = PP.text "return" <+> pp e
  pp (FunctionDef name args retType block) =
    PP.text "function" -- TODO: refine

instance PP Block where
  pp (Block [s]) = pp s
  pp (Block ss) = PP.vcat (Prelude.map pp ss)
  
instance PP Expression where
  pp (Val v) = pp v
  pp (Var var) = pp var
  pp (Op1 uop e) = pp uop <+> pp e
  pp (Op2 e1 bop e2) = pp e1 <+> pp bop <+> pp e2
  pp (Call fn es) = pp fn <> PP.parens (PP.hsep (PP.punctuate PP.comma (fmap pp es)))