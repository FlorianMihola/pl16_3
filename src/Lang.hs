module Lang
       where

data GoodNoise = Whitespace String
               | Comment String


newtype Name = Name String
             deriving (Show)

data ExprBase = StringLiteral String
                | BlockExpr Block
                | Reference (Name, Int)
                | ChildExpr Expr
                deriving (Show)

data SingleExpr = SingleExpr ExprBase [Selector]
                deriving (Show)

newtype Selector = Selector String
                 deriving (Show)

type Expr = [SingleExpr]

data Command = Guarded Guard [Command]
             | SimpleCommand Expr
             | Assignment (Name, Int) Expr
             | Return Expr
             deriving (Show)

newtype Block = Block [Command]
              deriving (Show)

newtype Guard = Guard [GuardExpr]
              deriving (Show)

-- GuardExpr a b n
-- a & b are expressions
-- n stands for negate
-- a = b -> a b False
-- a # b -> a b True
data GuardExpr = GuardExpr Expr Expr Bool
               deriving (Show)
