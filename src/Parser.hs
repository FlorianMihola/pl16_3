module Parser
       where

import           Lang
import           Text.Parsec.Prim       ( Parsec )
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Control.Applicative


type Parser a = Parsec String () a

nameChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

escapeChar = '\\' -- not in spec

escaped :: [Char] -> Parser Char
escaped escapableChars = do
  char escapeChar
  oneOf escapableChars

comment :: Parser GoodNoise
comment =
  Comment <$> (char '%' *> (many $ noneOf "\n\r") <* (many1 $ oneOf "\n\r"))

whitespace :: Parser GoodNoise
whitespace =
  Whitespace <$> many space

exprBase :: Parser ExprBase
exprBase =
  choice [ stringLiteral
         , BlockExpr <$> block
         , Reference <$> nameWithLevel
         , ChildExpr <$> between (char '(') (char ')') expr
         ]

singleExpr :: Parser SingleExpr
singleExpr = do
  SingleExpr <$> exprBase <*> many selector

selector :: Parser Selector
selector =
  Selector <$> (char '.' *> name)

expr :: Parser Expr
expr =
  ((\e -> [e]) <$> singleExpr) `chainl1` (char '+' *> return (++))

  {-
  choice [ Expr <$> singleExpr
         , concatExpr
         ]

sepBy2 p sep = do
  first <- p
  sep
  rest <- sepBy1 p sep
  return $ first : rest

concatExpr :: Parser Expr
concatExpr =
  ConcatExpr <$> ((\e -> [e]) <$> expr) `chainl1` (char '+' *> return (++))
-}

stringLiteral :: Parser ExprBase
stringLiteral =
  let
    string = many $ choice [ escaped  "\""
                           , noneOf "\""
                           ]
  in
    do
      StringLiteral <$> between (char '"') (char '"') string

block :: Parser Block
block =
  Block <$> between (char '{') (char '}') (many command)

command :: Parser Command
command =
  choice [ guarded
         , assignment
         , return'
         ]

guarded :: Parser Command
guarded = do
  char '['
  g <- guard
  char ':'
  cs <- many command
  char ']'
  return $ Guarded g cs

assignment :: Parser Command
assignment = do
  constructor <- option SimpleCommand (Assignment <$> (nameWithLevel <* char '='))
  let constructor = SimpleCommand
  e <- expr
  char ';'
  return $ constructor e

nameWithLevel :: Parser (Name, Int)
nameWithLevel = do
  as <- many $ char '*'
  n <- name
  return $ (Name n, length as)

name :: Parser String
name =
  many1 $ oneOf nameChars

return' :: Parser Command
return' =
  Return <$> (char '^' *> expr <* char ';')

guard :: Parser Guard
guard =
  Guard <$> sepBy1 guardExpr (char ',')

guardExpr :: Parser GuardExpr
guardExpr =
  let
    toNegate '=' = False
    toNegate '#' = True
  in
    do
      a <- expr
      n <- toNegate <$> oneOf "=#"
      b <- expr
      return $ GuardExpr a b n
