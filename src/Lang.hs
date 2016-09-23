module Lang
       where

import           Render
import           Parser.Char
import           Render.Tagged
import           Editable.String
import qualified Render.Tagged.Tag as Tag
import qualified Data.List         as List

data GoodNoise = Whitespace String
               | Comment String
               deriving (Show)

newtype Noise = Noise [GoodNoise]
              deriving (Show)


newtype Name = Name String
             deriving (Show)

data NameWithLevel = NameWithLevel Name Noise Int
                   deriving (Show)

data ExprBase = StringLiteral String
              | BlockExpr Block
              | Reference NameWithLevel
              | ChildExpr Noise Expr Noise
              deriving (Show)

data SingleExpr = SingleExpr Noise ExprBase [Either GoodNoise Selector]
                deriving (Show)

data Selector = Selector Noise String
              deriving (Show)

newtype Expr = Expr [SingleExpr]
             deriving (Show)

data Command = Guarded Noise Guard Noise [Either GoodNoise Command]
             | GuardedGarbage String
             | SimpleCommand Expr Noise
             | SimpleCommandGarbage String
             | Assignment NameWithLevel Noise Noise Expr Noise
             | Return Noise Expr Noise
             deriving (Show)

newtype Block = Block [Either GoodNoise Command]
              deriving (Show)

data Program = Program Noise Block Noise
             deriving (Show)

newtype Guard = Guard [GuardExpr]
              deriving (Show)

-- GuardExpr a n b
-- a & b are expressions
-- n stands for negate
-- a = b -> a b False
-- a # b -> a b True
data GuardExpr = GuardExpr Noise Expr Noise Bool Noise Expr Noise
               deriving (Show)

instance ToString Program where
  renderString (Program n block n') =
    renderString n ++ renderString block ++ renderString n'

instance ToString Noise where
  renderString (Noise noises) =
    renderString noises

instance ToString GoodNoise where
  renderString (Whitespace s) =
    s
  renderString (Comment s) =
    "%" ++ s ++ "\n"

instance ToString Block where
  renderString (Block xs) =
    "{" ++ (concat $ map renderStringE xs) ++ "}"

instance ToString Command where
  renderString (Guarded n g n' xs) =
    "[" ++ renderString n ++ renderString g ++ ":"
    ++ renderString n' ++ (concat $ map renderStringE xs) ++ "]"
  renderString (GuardedGarbage g) =
    "[" ++ g ++ "]"
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


instance ToTagged GoodNoise where
  toTagged (Whitespace s) =
    [Tagged Tag.Whitespace False $ fromString s]
  toTagged (Comment s) =
    [Tagged Tag.Comment False $ fromString $ '%' : s ++ "\n"]

instance ToTagged Noise where
  toTagged (Noise noises) =
    toTagged noises

instance ToTagged Program where
  toTagged (Program n b n') =
    toTagged n
    ++ toTagged b
    ++ toTagged n'

instance ToTagged Block where
  toTagged (Block xs) =
    [Tagged Tag.Block False $ fromString "{"]
    ++ toTagged xs
    ++ [Tagged Tag.Block False $ fromString "}"]

instance ToTagged Command where
  toTagged (Guarded n g n' xs) =
    [Tagged Tag.Guarded False $ fromString "["]
    ++ toTagged n
    ++ toTagged g
    ++ [Tagged Tag.Guarded False $ fromString ":"]
    ++ toTagged n'
    ++ toTagged xs
    ++ [Tagged Tag.Guarded False $ fromString "]"]
  toTagged (GuardedGarbage s) =
    [ Tagged Tag.GuardedGarbage False $ fromString "["
    , Tagged Tag.Garbage False $ fromString s
    , Tagged Tag.GuardedGarbage False $ fromString "]"
    ]
  toTagged (SimpleCommand e n) =
    toTagged e ++ toTagged n
  toTagged (SimpleCommandGarbage s) =
    [ Tagged Tag.Garbage False $ fromString s
    , Tagged Tag.Command False $ fromString ";"
    ]
  toTagged (Assignment name n n' e n'') =
    toTagged name
    ++ toTagged n
    ++ [Tagged Tag.Assignment False $ fromString "="]
    ++ toTagged n'
    ++ toTagged e
    ++ toTagged n''
    ++ [Tagged Tag.Command False $ fromString ";"]
  toTagged (Return n e n') =
    [Tagged Tag.Return False $ fromString "^"]
    ++ toTagged n
    ++ toTagged e
    ++ toTagged n'

instance ToTagged NameWithLevel where
  toTagged (NameWithLevel name n level) =
    [Tagged Tag.Level False $ fromString $ concat $ take level $ repeat "*"]
    ++ toTagged n
    ++ toTagged name

instance ToTagged Expr where
  toTagged (Expr xs) =
    toTagged xs

instance ToTagged SingleExpr where
  toTagged (SingleExpr n eb xs) =
    toTagged n
    ++ toTagged eb
    ++ toTagged xs

instance ToTagged Selector where
  toTagged (Selector n s) =
    [Tagged Tag.Selector False $ fromString "."]
    ++ toTagged n
    ++ [Tagged Tag.Name False $ fromString s]

instance ToTagged ExprBase where
  toTagged (StringLiteral s) =
    [Tagged Tag.String False $ fromString $ "\"" ++ s ++ "\""]
  toTagged (BlockExpr b) =
    toTagged b
  toTagged (Reference name) =
    toTagged name
  toTagged (ChildExpr n e n') =
    [Tagged Tag.String False $ fromString "("]
    ++ toTagged n
    ++ toTagged e
    ++ toTagged n'
    ++ [Tagged Tag.String False $ fromString ")"]

instance ToTagged Guard where
  toTagged (Guard xs) =
    toTagged xs

instance ToTagged Name where
  toTagged (Name s) =
    [Tagged Tag.Name False $ fromString s]

instance ToTagged GuardExpr where
  toTagged (GuardExpr an a an' n bn b bn') =
    toTagged an
    ++ toTagged a
    ++ toTagged an
    ++ [ if n
           then
             Tagged Tag.NotEqual False $ fromString "#"
           else
             Tagged Tag.Equal False $ fromString "="
       ]
    ++ toTagged bn
    ++ toTagged b
    ++ toTagged bn'
