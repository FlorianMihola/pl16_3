module Render.Curses
       where

import           UI.NCurses

class PrintCurses a where
  printCurses :: a -> Update ()

class PrintFocusedCurses a where
  printFocusedCurses :: ColorID -> a -> Update ()
