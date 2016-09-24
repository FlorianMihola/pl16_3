module Render.Tagged.Tag
       where

import           System.Console.ANSI
import           UI.NCurses

data Tag = Whitespace
         | Comment
         | Block
         | Garbage
         | Guarded
         | GuardedGarbage
         | Command
         | Assignment
         | AssignmentNotRead
         | Return
         | Level
         | String
         | Selector
         | Name
         | NameNotAssigned
         | Equal
         | NotEqual
         | Cursor
         | Plus
         | Default
         | Unknown
         | Unparsed
         deriving ( Show
                  , Ord
                  , Eq
                  )

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

tagCursesColors =
  [ (Default,            1, ColorDefault, ColorDefault)
  , (Whitespace,         2, ColorDefault, ColorDefault)
  , (Garbage,            3, ColorBlack,   ColorRed)
  , (Cursor,             4, ColorBlack,   ColorWhite)
  , (Comment,            5, ColorYellow,  ColorDefault)
  , (Block,              6, ColorMagenta, ColorDefault)
  , (Garbage,            7, ColorBlack,   ColorRed)
  , (Guarded,            8, ColorBlue,    ColorDefault)
  , (GuardedGarbage,     9, ColorRed,     ColorDefault)
  , (Command,           10, ColorMagenta, ColorDefault)
  , (Assignment,        11, ColorRed,     ColorDefault)
  , (AssignmentNotRead, 19, ColorBlack,   ColorMagenta)
  , (Return,            12, ColorMagenta, ColorDefault)
  , (Level,             13, ColorMagenta, ColorDefault)
  , (String,            14, ColorGreen,   ColorDefault)
  , (Selector,          15, ColorBlue,    ColorDefault)
  , (Name,              16, ColorCyan,    ColorDefault)
  , (Equal,             17, ColorYellow,  ColorDefault)
  , (NotEqual,          18, ColorMagenta, ColorDefault)
  , (NameNotAssigned,   21, ColorBlack,   ColorCyan)
  , (Plus,              22, ColorGreen,   ColorDefault)
  , (Unparsed,          20, ColorDefault, ColorDefault)
  -- Unknown will never be rendered
  ]
