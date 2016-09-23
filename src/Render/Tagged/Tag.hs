module Render.Tagged.Tag
       where

import           System.Console.ANSI

data Tag = Whitespace
         | Comment
         | Block
         | Garbage
         | Guarded
         | GuardedGarbage
         | Command
         | Assignment
         | Return
         | Level
         | String
         | Selector
         | Name
         | Equal
         | NotEqual
         deriving (Show)

tagSGR :: Tag -> [SGR]
tagSGR Whitespace =
  []
tagSGR Comment =
  [ SetColor Foreground Dull Yellow
  , SetItalicized True
  ]
tagSGR Block =
  [ SetColor Foreground Dull White ]
tagSGR Garbage =
  [ SetColor Background Dull Red ]
tagSGR Guarded =
  [ SetColor Foreground Vivid Blue ]
tagSGR GuardedGarbage =
  [ SetColor Foreground Dull Blue ]
tagSGR Command =
  [ SetColor Foreground Dull Yellow ]
tagSGR Assignment =
  [ SetColor Foreground Dull Magenta ]
tagSGR Return =
  [ SetColor Foreground Vivid Green ]
tagSGR Level =
  [ SetColor Foreground Vivid Cyan]
tagSGR String =
  [ SetColor Foreground Dull Green ]
tagSGR Selector =
  [ SetColor Foreground Vivid Magenta ]
tagSGR Name =
  [ SetColor Foreground Vivid Red ]
tagSGR Equal =
  [ SetColor Foreground Dull Cyan ]
tagSGR NotEqual =
  [ SetColor Foreground Dull Cyan ]
