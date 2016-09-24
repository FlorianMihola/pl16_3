module Main
       where

import           Text.Parsec.Prim       ( runParser )
import           Text.Parsec.String     ( parseFromFile )
import           Parser
import           Render
import           Render.ANSI
import           Render.Curses
import           Editable               hiding (forward, backward, insert)
import           Editable.String
import           Editable.List
--import           Editable.Instances
import           Render.Tagged
import           Render.Tagged.Tag      as Tag
import           Buffer               --hiding (forward, backward, insert)
import           System.Environment
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
        e <- parseFromFile program fileName
        case e of
          Left err ->
            putStrLn $ "Parse error: " ++ show err
          Right p -> do
            let tagged = toTagged p
            let tagged' = fromJust $ (Just $ Buffer $ fromList $ focusFirst tagged)

            --hSetBuffering stdin NoBuffering
            --hSetEcho stdin False

            let loop w colorIDs fileName buffer event = do
                  let continue buffer = getEvent w Nothing >>= loop w colorIDs fileName buffer
                  let buffer' =
                        case event of
                          Just (EventSpecialKey KeyRightArrow) ->
                            forward buffer
                          Just (EventSpecialKey KeyLeftArrow) ->
                            backward buffer
                          Just (EventSpecialKey KeyDownArrow) ->
                            down buffer
                          Just (EventSpecialKey KeyUpArrow) ->
                            up buffer
                          Just (EventSpecialKey KeyDeleteCharacter) ->
                            delete buffer
                          Just (EventSpecialKey KeyBackspace) ->
                            backspace buffer
                          Just (EventCharacter '\ETB') ->
                            buffer
                          Just (EventCharacter c) ->
                            insert buffer c
                          _ ->
                            buffer
                  case event of
                    Just (EventCharacter '\ETB') ->
                      liftIO $ writeFile fileName $ renderString buffer
                    _ ->
                      return ()
                  updateWindow w $ do
                    moveCursor 0 0
                    drawString $ show event
                    moveCursor 1 0
                    renderBuffer colorIDs buffer'
                    moveCursor 20 0
                    printDebug buffer'
                  render
                  continue buffer'

            runCurses $ do
              w <- defaultWindow
              colorIDs <- Map.fromList
                          <$> mapM (\(t, i, fg, bg) ->
                                     do
                                       id <- newColorID fg bg i
                                       return (t, id)
                                   ) tagCursesColors
              updateWindow w $ do
                moveCursor 0 0
                renderBuffer colorIDs tagged'
              render
              getEvent w Nothing >>= loop w colorIDs fileName tagged'
    else
      putStrLn "Please specify which file to read."

renderBuffer colorIDs (Buffer l) = do
  mapM_ (renderTagged colorIDs) $ toList l
  case l of
    (List _ []) -> do
      setColor' colorIDs Tag.Cursor
      drawString " "
    _ ->
      return ()

renderTagged colorIDs (Tagged tag focused es) =
  if focused
    then
      renderFocusedES colorIDs tag es
    else
      do
        setColor' colorIDs tag
        drawString $ renderString es

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
  drawString $ show $ findFocused $ toList l
  --mapM (drawString . show) $ take 5 $ toList l

findFocused [] = Nothing
findFocused (h@(Tagged _ True _) : ts) = Just h
findFocused (h@(Tagged _ False _) : ts) = findFocused ts
