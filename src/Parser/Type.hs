module Parser.Type
       where

import           Text.Parsec.Prim       ( Parsec )

type Parser a = Parsec String () a
