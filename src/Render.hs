module Render
       where

import           Lang
import           Parser
import qualified Data.List as List

class ToString a where
  renderString :: a -> String

renderStringE (Left x)  = renderString x
renderStringE (Right x) = renderString x

instance ToString Program where
  renderString (Program n block n') =
    renderString n ++ renderString block ++ renderString n'

instance ToString Noise where
  renderString (Noise noises) =
    concat $ map renderString noises

instance ToString GoodNoise where
  renderString (Whitespace s) = s
  renderString (Comment c)    = "%" ++ c ++ "\n"

instance ToString Block where
  renderString (Block xs) =
    "{" ++ (concat $ map renderStringE xs) ++ "}"

instance ToString Command where
  renderString (Guarded n g n' xs) =
    "[" ++ renderString n ++ renderString g ++ ":"
    ++ renderString n' ++ (concat $ map renderStringE xs) ++ "]"
  renderString (SimpleCommand e n) =
    renderString e ++ renderString n ++ ";"
  renderString (Assignment name n n' e n'') =
    renderString name ++ renderString n ++ "=" ++ renderString n'
    ++ renderString e ++ renderString n'' ++ ";"
  renderString (Return n e n') =
    "^" ++ renderString n ++ renderString e ++ renderString n' ++ ";"

instance ToString Guard where
  renderString (Guard xs) =
    concat $ List.intersperse "," $ map renderString xs

instance ToString GuardExpr where
  renderString (GuardExpr an a an' neg bn b bn') =
    renderString an ++ renderString a ++ renderString an'
    ++ (if neg then "#" else "=")
    ++ renderString bn ++ renderString b ++ renderString bn'

instance ToString Expr where
  renderString (Expr xs) =
    concat $ List.intersperse "+" $ map renderString xs

instance ToString SingleExpr where
  renderString (SingleExpr n b xs) =
    renderString n ++ renderString b ++ (concat $ map renderStringE xs)

instance ToString ExprBase where
  renderString (StringLiteral s) =
    "\"" ++ escape s ++ "\""
  renderString (BlockExpr b) =
    renderString b
  renderString (Reference n) =
    renderString n
  renderString (ChildExpr n e n') =
    "(" ++ renderString n ++ renderString e ++ renderString n' ++ ")"

instance ToString Selector where
  renderString (Selector n s) =
    "." ++ renderString n ++ s

instance ToString NameWithLevel where
  renderString (NameWithLevel name n level) =
    (concat $ take level $ repeat "*")
    ++ renderString n ++ renderString name

instance ToString Name where
  renderString (Name s) = s