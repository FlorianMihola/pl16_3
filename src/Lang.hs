module Lang
       where

import           PreludePlus
import           Render
import           Parser.Char
import           Render.Tagged
import           Editable.String
--import           ReadNames
import qualified Render.Tagged.Tag as Tag
import qualified Data.List         as List
import           Data.Maybe

data GoodNoise = Whitespace String
               | Comment String Bool
               deriving (Show, Eq)

newtype Noise = Noise [GoodNoise]
              deriving (Show, Eq)

data Name = Name String
          | NameGarbage String Noise String
          deriving (Show, Eq)

data NameWithLevel = NameWithLevel Name Noise Int
                   deriving (Show)

data ExprBase = StringLiteral String
              | BlockExpr Block
              | Reference NameWithLevel
              | ChildExpr Noise Expr Noise
              deriving (Show)

data SingleExpr = SingleExpr Noise ExprBase [Either GoodNoise Selector]
                deriving (Show)

data Selector = Selector Noise Name
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
             | ProgramGarbage String
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
  renderString (ProgramGarbage s) =
    s

instance ToString Noise where
  renderString (Noise noises) =
    renderString noises

instance ToString GoodNoise where
  renderString (Whitespace s) =
    s
  renderString (Comment s newline) =
    "%" ++ s ++ (if newline then "\n" else "")

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
    "." ++ renderString n ++ renderString s

instance ToString NameWithLevel where
  renderString (NameWithLevel name n level) =
    (concat $ take level $ repeat "*")
    ++ renderString n ++ renderString name

instance ToString Name where
  renderString (Name s) = s


instance ToTagged GoodNoise where
  toTagged (Whitespace s) =
    [Tagged Tag.Whitespace False $ fromString s]
  toTagged (Comment s newline) =
    [Tagged Tag.Comment False
       $ fromString $ '%' : s ++ (if newline then "\n" else "")
    ]

instance ToTagged Noise where
  toTagged (Noise noises) =
    toTagged noises

instance ToTagged Program where
  toTagged (Program n b n') =
    toTagged n
    ++ toTagged b
    ++ toTagged n'
  toTagged (ProgramGarbage s) =
    [Tagged Tag.Garbage False $ fromString s]

instance ToTagged Block where
  toTagged b@(Block xs) =
    [Tagged Tag.Block False $ fromString "{"]
    ++ (concat $ map (toTagged' (readNames b)) xs)
    ++ [Tagged Tag.Block False $ fromString "}"]

elem' (NameWithLevel n _ l) names =
  let
    p (NameWithLevel n' _ l') =
      n == n' && l == l'
  in
    isJust $ List.find p names

toTagged' names x =
  case x of
    Left x' ->
      toTagged x'
    Right a@(Assignment name n n' e n'') ->
      (if name `elem'` names
         then
           toTagged name
         else
           [Tagged Tag.AssignmentNotRead False $ fromString $ renderString name]
      )
      ++ toTagged n
      ++ [Tagged Tag.Assignment False $ fromString "="]
      ++ toTagged n'
      ++ toTagged e
      ++ toTagged n''
      ++ [Tagged Tag.Command False $ fromString ";"]
    Right x' ->
      toTagged x'

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
    ++ toTagged s

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
  toTagged name@(NameGarbage s n s') =
    [Tagged Tag.Garbage False $ fromString $ renderString name]

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


class ReadNames a where
  readNames :: a -> [NameWithLevel]

instance ReadNames Block where
  readNames (Block xs) =
    concat $ map (readNames . fromRight) $ filter isRight xs

instance ReadNames Command where
  readNames (SimpleCommand e _) =
    readNames e
  readNames (Assignment _ _ _ e _) =
    readNames e
  readNames _ =
   []
{-         Guarded Noise Guard Noise [Either GoodNoise Command]
             | GuardedGarbage String
             | SimpleCommandGarbage String
             | Assignment NameWithLevel Noise Noise Expr Noise
             | Return Noise Expr Noise
-}


instance ReadNames Expr where
  readNames (Expr xs) =
    concat $ map readNames xs

instance ReadNames SingleExpr where
  readNames (SingleExpr _ eb _) =
    readNames eb

instance ReadNames ExprBase where
  readNames (Reference name) =
    [name]
  readNames (StringLiteral _) =
    []
  readNames (BlockExpr b) =
    map adaptLevel $ readNames b
  readNames (ChildExpr _ e _) =
   readNames e

adaptLevel (NameWithLevel name n l) =
  NameWithLevel name n (l - 1)

class AssignedNames a where
  assignedNames :: a -> [NameWithLevel]

instance AssignedNames Block where
  assignedNames (Block xs) =
    concat $ map (assignedNames . fromRight) $ filter isRight xs

instance AssignedNames Command where
  assignedNames (Assignment name _ _ e _) =
    [name]
  assignedNames _ =
   []

  {-assignedNames (SimpleCommand e _) =
    assignedNames e-}
{-         Guarded Noise Guard Noise [Either GoodNoise Command]
             | GuardedGarbage String
             | SimpleCommandGarbage String
             | Assignment NameWithLevel Noise Noise Expr Noise
             | Return Noise Expr Noise
-}
{-
instance AssignedNames Expr where
  assignedNames (Expr xs) =
    concat $ map assignedNames xs

instance AssignedNames SingleExpr where
  assignedNames (SingleExpr _ eb _) =
    assignedNames eb

instance AssignedNames ExprBase where
  assignedNames (Reference name) =
    [name]
  assignedNames (StringLiteral _) =
    []
  assignedNames (BlockExpr b) =
    map adaptLevel $ assignedNames b
  assignedNames (ChildExpr _ e _) =
   assignedNames e
-}

class ToTaggedA a where
  toTaggedA :: [NameWithLevel] -> a -> [Tagged]

instance ToTaggedA a => ToTaggedA [a] where
  toTaggedA names =
    concat . map (toTaggedA names)

instance (ToTaggedA a, ToTaggedA b) => ToTaggedA (Either a b) where
  toTaggedA names (Left x) =
    toTaggedA names x
  toTaggedA names (Right x) =
    toTaggedA names x


instance ToTaggedA GoodNoise where
  toTaggedA _ = toTagged

instance ToTaggedA Noise where
  toTaggedA _ = toTagged

instance ToTaggedA Program where
  toTaggedA _ (Program n b n') =
    toTagged n
    ++ toTaggedA (assignedNames b) b
    ++ toTagged n'
  toTaggedA _ pg@(ProgramGarbage s) =
    toTagged pg

instance ToTaggedA Block where
  toTaggedA names b@(Block xs) =
    [Tagged Tag.Block False $ fromString "{"]
    ++ (concat $ map (toTaggedA' names (readNames b)) xs)
    ++ [Tagged Tag.Block False $ fromString "}"]

toTaggedA' assignedNames readNames x =
  case x of
    Left x' ->
      toTagged x'
    Right a@(Assignment name n n' e n'') ->
      (if name `elem'` readNames
         then
           toTagged name
         else
           [Tagged Tag.AssignmentNotRead False $ fromString $ renderString name]
      )
      ++ toTagged n
      ++ [Tagged Tag.Assignment False $ fromString "="]
      ++ toTagged n'
      ++ toTaggedA assignedNames e
      ++ toTagged n''
      ++ [Tagged Tag.Command False $ fromString ";"]
    Right x' ->
      toTagged x'
{-
instance ToTaggedA Command where
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
-}
instance ToTaggedA NameWithLevel where
  toTaggedA names name@(NameWithLevel name' n level) =
    if name `elem'` names
      then
        toTagged name
      else
        [Tagged Tag.NameNotAssigned False $ fromString $ renderString name]


instance ToTaggedA Expr where
  toTaggedA names (Expr xs) =
    toTaggedA names xs

instance ToTaggedA SingleExpr where
  toTaggedA names (SingleExpr n eb xs) =
    toTagged n
    ++ toTaggedA names eb
    ++ toTaggedA names xs

instance ToTaggedA Selector where
  toTaggedA _ = toTagged

instance ToTaggedA ExprBase where
  toTaggedA _ sl@(StringLiteral s) =
    toTagged sl
  toTaggedA names (BlockExpr b) =
    toTaggedA names b
  toTaggedA names r@(Reference name) =
    toTaggedA names name
  toTaggedA names (ChildExpr n e n') =
    [Tagged Tag.String False $ fromString "("]
    ++ toTagged n
    ++ toTaggedA names e
    ++ toTagged n'
    ++ [Tagged Tag.String False $ fromString ")"]
{-
instance ToTaggedA Guard where
  toTagged (Guard xs) =
    toTagged xs

instance ToTaggedA Name where
  toTagged (Name s) =
    [Tagged Tag.Name False $ fromString s]
  toTagged name@(NameGarbage s n s') =
    [Tagged Tag.Garbage False $ fromString $ renderString name]

instance ToTaggedA GuardExpr where
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

-}
