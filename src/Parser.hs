module Parser
       where

import           Lang
import           Text.Parsec.Prim       ( Parsec
                                        , try
                                        , (<?>)
                                        )
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Control.Applicative
import qualified Data.List              as List

type Parser a = Parsec String () a

nameChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

escapeChar = '\\' -- not in spec

escapableChars = "\""

escaped :: [Char] -> Parser Char
escaped escapableChars = do
  char escapeChar
  oneOf escapableChars

escape :: String -> String
escape = List.foldr (\c s -> if List.elem c escapableChars
                             then escapeChar : c : s
                             else c : s
                    )
         ""

comment :: Parser GoodNoise
comment =
  Comment <$> (char '%' *> (many $ noneOf "\n\r") <* (many1 $ oneOf "\n\r"))
  <?> "comment"

whitespace :: Parser GoodNoise
whitespace =
  Whitespace <$> many1 space
  <?> "whitespace"

goodNoise :: Parser GoodNoise
goodNoise =
  choice [ comment
         , whitespace
         ]

noise :: Parser Noise
noise =
  Noise <$> many goodNoise

expr :: Parser Expr
expr =
  --Expr <$> ((\e -> [e]) <$> singleExpr) `chainl1` (char '+' *> return (++))
  Expr <$> sepBy1 singleExpr (char '+')

{-
exprOrGargabe :: Parser (Either Gargabe Expr)
exprOrGargabe =
  choice [ Left <$>  garbage (char ';')
         , Right <$> expr
         ]
-}

{-
singleExprOrNoise :: Parser (Either GoodNoise SingleExpr)
singleExprOrNoise =
  choice [ Left  <$> goodNoise
         , Right <$> singleExpr
         ]
-}

singleExpr :: Parser SingleExpr
singleExpr = do
  n <- noise
  b <- exprBase
  s <- many selectorOrNoise
  return $ SingleExpr n b s

exprBase :: Parser ExprBase
exprBase =
  choice [ stringLiteral
         , BlockExpr <$> block
         , Reference <$> nameWithLevel
         , childExpr
         ]

stringLiteral :: Parser ExprBase
stringLiteral =
  let
    string = many $ choice [ escaped escapableChars
                           , noneOf escapableChars
                           ]
  in
   StringLiteral <$> between (char '"') (char '"') string

childExpr :: Parser ExprBase
childExpr = do
  char '('
  preNoise <- noise
  e <- expr
  postNoise <- noise
  char ')'
  return $ ChildExpr preNoise e postNoise

selectorOrNoise :: Parser (Either GoodNoise Selector)
selectorOrNoise =
  choice [ Left  <$> goodNoise
         , Right <$> selector
         ]

selector :: Parser Selector
selector = do
  char '.'
  n <- noise
  s <- name
  return $ Selector n s

block :: Parser Block
block =
  between (char '{') (char '}') nakedBlock

nakedBlock :: Parser Block
nakedBlock =
  Block <$> many commandOrNoise

program :: Parser Program
program = do
  preNoise <- noise
  b <- block
  postNoise <- noise
  return $ Program preNoise b postNoise

commandOrNoise :: Parser (Either GoodNoise Command)
commandOrNoise =
  choice [ Left  <$> goodNoise
         , Right <$> command
         ]

command :: Parser Command
command =
  {-choice [ try -} commandProper <?> "command"
{-         , commandGarbage
         ]-}

commandProper :: Parser Command
commandProper =
  choice [ guarded
         , try assignment
         , simpleCommand
         , return'
         ]

commandGarbage :: Parser Command
commandGarbage =
  simpleCommandGarbage

guarded :: Parser Command
guarded =
  choice [ try guardedProper
         , guardedGarbage
         ]

guardedProper :: Parser Command
guardedProper = do
  char '['
  preNoise <- noise
  g <- guard
  postNoise <- noise
  char ':'
  cs <- many commandOrNoise
  char ']'
  return $ Guarded preNoise g postNoise cs

guardedGarbage :: Parser Command
guardedGarbage =
  GuardedGarbage <$> (char '[' *> (many $ noneOf "]") <* char ']')

simpleCommand :: Parser Command
simpleCommand =
  choice [ try simpleCommandProper
         , simpleCommandGarbage
--         , try simpleCommandGarbage'
         ]

simpleCommandProper :: Parser Command
simpleCommandProper =
  SimpleCommand <$> expr <*> noise <* char ';'

simpleCommandGarbage :: Parser Command
simpleCommandGarbage =
  SimpleCommandGarbage <$> ((many $ noneOf "]};") <* char ';')

{-
simpleCommandGarbage' :: Parser Command
simpleCommandGarbage' =
  SimpleCommandGarbage <$> ((many $ noneOf ";") <* char ';')-}


assignment :: Parser Command
assignment = do
  n <- nameWithLevel
  postNameNoise <- noise
  char '='
  preExprNoise <- noise
  e <- expr
  postExprNoise <- noise
  char ';'
  return $ Assignment n postNameNoise preExprNoise e postExprNoise

nameWithLevel :: Parser NameWithLevel
nameWithLevel = do
  as <- many $ char '*'
  postAsteriskNoise <- noise
  n <- name
  return $ NameWithLevel (Name n) postAsteriskNoise (length as)

name :: Parser String
name =
  many1 $ oneOf nameChars

return' :: Parser Command
return' = do
  char '^'
  preNoise <- noise
  e <- expr
  postNoise <- noise
  char ';'
  return $ Return preNoise e postNoise

guard :: Parser Guard
guard =
  Guard <$> sepBy1 guardExpr (char ',')

{-
guardExprOrNoise :: Parser (Either GoodNoise GuardExpr)
guardExprOrNoise =
  choice [ Left  <$> goodNoise
         , Right <$> guardExpr
         ]
-}

guardExpr :: Parser GuardExpr
guardExpr =
  let
    toNegate '=' = False
    toNegate '#' = True
  in
    do
      preANoise <- noise
      a <- expr
      postANoise <- noise
      n <- toNegate <$> oneOf "=#"
      preBNoise <- noise
      b <- expr
      postBNoise <- noise
      return $ GuardExpr preANoise a postANoise n preBNoise b postBNoise


{-garbage :: Parser Garbage
garbage = string
-}
