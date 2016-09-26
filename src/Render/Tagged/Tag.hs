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
         | ChildExpr
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
         | Padding
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
  zipWith (\i (t, fg, bg) -> (t, i, fg, bg)) [1..] tagCursesColors'

tagCursesColors' =
  [ (Default,            ColorDefault, ColorDefault)
  , (Whitespace,         ColorDefault, ColorDefault)
  , (Garbage,            ColorBlack,   ColorRed)
  , (Cursor,             ColorBlack,   ColorWhite)
  , (Comment,            ColorYellow,  ColorDefault)
  , (Block,              ColorMagenta, ColorDefault)
  , (Garbage,            ColorBlack,   ColorRed)
  , (Guarded,            ColorBlue,    ColorDefault)
  , (GuardedGarbage,     ColorRed,     ColorDefault)
  , (Command,            ColorMagenta, ColorDefault)
  , (Assignment,         ColorRed,     ColorDefault)
  , (AssignmentNotRead,  ColorBlack,   ColorMagenta)
  , (Return,             ColorMagenta, ColorDefault)
  , (Level,              ColorMagenta, ColorDefault)
  , (String,             ColorGreen,   ColorDefault)
  , (Selector,           ColorBlue,    ColorDefault)
  , (Name,               ColorCyan,    ColorDefault)
  , (Equal,              ColorYellow,  ColorDefault)
  , (NotEqual,           ColorMagenta, ColorDefault)
  , (NameNotAssigned,    ColorBlack,   ColorCyan)
  , (Plus,               ColorBlue,    ColorDefault)
  , (Unparsed,           ColorDefault, ColorDefault)
  , (ChildExpr,          ColorRed,     ColorDefault)
  , (Padding,            ColorDefault, ColorBlack)
  -- Unknown will never be rendered
  ]
