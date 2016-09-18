module Lang
       where

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
             | SimpleCommand Expr Noise
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
