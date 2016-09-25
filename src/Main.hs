module Main
       where

import           Text.Parsec.Prim       ( runParser )
import           Text.Parsec.String     ( parseFromFile )
import           Lang
import           Parser
import           Render
import           Render.ANSI
import           Render.Curses
import           Editable               hiding (forward, backward, insert)
import qualified Editable               as E
import           Editable.String
import           Editable.List
--import           Editable.Instances
import           Render.Tagged
import           Render.Tagged.Tag      as Tag
import           Buffer               --hiding (forward, backward, insert)
import           System.Environment
import           System.Directory
import           Data.Maybe
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           System.IO
import           Data.Char
import           UI.NCurses
import qualified Data.Map.Strict        as Map

main = do
  args <- getArgs
  if length args == 1
    then
      do
        let fileName = (args !! 0)
        fileString <- readFile fileName

        let loop w colorIDs fileName buffer saved = do
              renderScreen w colorIDs fileName buffer saved
              event <- getEvent w Nothing
              (buffer, saved) <-
                    case event of
                      Just (EventSpecialKey KeyRightArrow) ->
                        return (forward buffer, saved)
                      Just (EventSpecialKey KeyLeftArrow) ->
                        return (backward buffer, saved)
                      Just (EventSpecialKey KeyDownArrow) ->
                        return (down buffer, saved)
                      Just (EventSpecialKey KeyUpArrow) ->
                        return (up buffer, saved)
                      Just (EventSpecialKey KeyDeleteCharacter) ->
                        return (delete buffer, False)
                      Just (EventSpecialKey KeyBackspace) ->
                        return (backspace buffer, False)
                      Just (EventCharacter '\ETB') ->
                        return (buffer, saved)
                      Just (EventCharacter '\CAN') ->
                        return (buffer, saved)
                      Just (EventCharacter '\SI') ->
                        return (buffer, saved)
                      Just (EventCharacter c) ->
                        return (insert buffer c, False)
                      _ ->
                        return (buffer, saved)

              saved <- if (event == Just (EventCharacter '\ETB'))
                         then
                           do -- Ctrl-W write file
                             liftIO $ writeFile fileName $ renderString buffer
                             return True
                         else
                           return saved

              (fileName, buffer) <-
                if (event == Just (EventCharacter '\SI'))
                  then
                    openFileDialog colorIDs w (fromString "")
                    >>= \(name, string) -> return (name, parseBuffer string)
                  else
                    return (fileName, buffer)

              when (event /= Just (EventCharacter '\CAN')) -- Ctrl-X quit
                   (loop w colorIDs fileName buffer saved)

        runCurses $ do
          setCursorMode CursorInvisible
          w <- defaultWindow
          colorIDs <- Map.fromList
                      <$> mapM (\(t, i, fg, bg) ->
                                 do
                                   id <- newColorID fg bg i
                                   return (t, id)
                               ) tagCursesColors
          let buffer = parseBuffer fileString
          renderScreen w colorIDs fileName buffer True
          loop w colorIDs fileName buffer True
    else
      putStrLn "Please specify which file to read."

renderScreen w colorIDs fileName buffer saved = do
  let o = offset buffer
  let buffer' = fromJust $ (if o > 0
                              then
                                foldl1 (\a b -> \x -> a x >>= b)
                                $ take o $ repeat E.forward
                              else
                                Just
                           ) $ parseBuffer $ renderString buffer
  (rows, columns) <- screenSize
  updateWindow w $ do
    clear
    setColor' colorIDs Tag.Default
    moveCursor 0 0
    drawString $ "File: " ++ fileName ++ (if saved then "" else " (modified)") 
    moveCursor 1 0
    renderBuffer colorIDs
                 0
                 (fromInteger $ rows - 2)
                 (fromInteger columns)
                 $ addCursor buffer'

    --moveCursor 20 0
    --printDebug buffer'
  render

renderBuffer colorIDs skipLines numLines numColumns buffer =
  let
    lines = toLines buffer
    lines' = concat $ map (splitLine "  " numColumns) lines
  in
    mapM_ (renderLine colorIDs)
          $ take numLines $ drop skipLines lines'

  {-case l of
    (List _ []) -> do
      setColor' colorIDs Tag.Cursor
      drawString " "
    _ ->
      return ()-}

renderLine colorIDs (Buffer l) =
  mapM_ (renderTagged colorIDs) $ toList l

renderTagged colorIDs (Tagged tag _ es) = do
  setColor' colorIDs tag
  drawString $ renderString es

{-renderTagged colorIDs (Tagged tag focused es) =
  if focused
    then
      renderFocusedES colorIDs tag es
    else
      do
        setColor' colorIDs tag
        drawString $ renderString es
-}

setColor' colorIDs tag =
  case Map.lookup tag colorIDs of
   Just color ->
     setColor color
   Nothing ->
     setColor (colorIDs Map.! Tag.Default)

renderFocusedES colorIDs tag (EditableString xs ys) = do
  setColor' colorIDs tag
  drawString $ reverse xs
  when
    (not $ null ys)
    (do
        setColor' colorIDs Tag.Cursor
        let yh = head ys
        if yh == '\n'
          then
            drawString $ " " ++ [yh]
          else
            drawString [yh]
        setColor' colorIDs tag
        drawString $ tail ys
    )

printDebug (Buffer l) = do
  drawString $ show $ {-findFocused $-} toList l

findFocused [] = Nothing
findFocused (h@(Tagged _ True _) : ts) = Just h
findFocused (h@(Tagged _ False _) : ts) = findFocused ts

openFileDialog colorIDs w fileName = do
  setCursorMode CursorVisible
  updateWindow w $ do
    clear
    setColor' colorIDs Tag.Default
    moveCursor 0 0
    drawString $ "Open file: "
    renderFocusedES colorIDs Tag.Default fileName
    when (atEnd fileName)
      (do
          setColor' colorIDs Tag.Cursor
          drawString " "
      )
  render
  setCursorMode CursorInvisible
  event <- getEvent w Nothing
  case event of
    Just (EventSpecialKey KeyRightArrow) ->
      let
        fileName' = maybe fileName id (E.forward fileName)
      in
        openFileDialog colorIDs w fileName'
    Just (EventSpecialKey KeyLeftArrow) ->
      let
        fileName' = maybe fileName id (E.backward fileName)
      in
        openFileDialog colorIDs w fileName'
    Just (EventSpecialKey KeyDownArrow) ->
      openFileDialog colorIDs w $ toEnd fileName
    Just (EventSpecialKey KeyUpArrow) ->
      openFileDialog colorIDs w $ toBeginning fileName
    Just (EventSpecialKey KeyDeleteCharacter) ->
      let
        fileName' = maybe fileName id (E.remove fileName)
      in
        openFileDialog colorIDs w fileName'
    Just (EventSpecialKey KeyBackspace) ->
      let
        fileName' = maybe fileName id (E.backward fileName >>= E.remove)
      in
        openFileDialog colorIDs w fileName'
    Just (EventCharacter '\n') -> do
      let filePath = renderString fileName
      exists <- liftIO $ doesFileExist filePath
      if exists
        then
          liftIO $ readFile filePath
          >>= \fileString -> return (filePath, fileString)
        else
          return (filePath, "")
    Just (EventCharacter c) ->
      let
        fileName' = maybe fileName id (E.insert fileName c)
      in
        openFileDialog colorIDs w fileName'
    _ ->
      openFileDialog colorIDs w fileName
