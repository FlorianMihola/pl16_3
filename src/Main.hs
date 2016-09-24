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
import           System.IO
import           Data.Char
import           UI.NCurses
import qualified Data.Map.Strict        as Map

main = do
  args <- getArgs
  if length args == 1
    then
      do
        e <- parseFromFile program (args !! 0)
        case e of
          Left err ->
            putStrLn $ "Parse error: " ++ show err
          Right p -> do
            let tagged = toTagged p
            --print p
            let tagged' = fromJust $ (Just $ Buffer $ fromList $ focusFirst tagged)
{-                          >>= forward >>= forward >>= forward
                          >>= flip insert 'X'
                          >>= flip insert 'Y'
                          >>= flip insert 'Z'
                          >>= Just . renderString
                          >>= either
                              (const Nothing)
                              (Just . Buffer . fromList . focusFirst . toTagged)
                              . runParser program () ""
            print tagged'-}

            hSetBuffering stdin NoBuffering
            hSetEcho stdin False

            let loop w colorIDs buffer event = do
                  let continue buffer = getEvent w Nothing >>= loop w colorIDs buffer
                  let buffer' =
                        case event of
                          Just (EventSpecialKey KeyRightArrow) ->
                            forward buffer
                          Just (EventSpecialKey KeyLeftArrow) ->
                            backward buffer
                          Just (EventSpecialKey KeyDeleteCharacter) ->
                            delete buffer
                          Just (EventSpecialKey KeyBackspace) ->
                            backspace buffer
                          Just (EventCharacter c) ->
                            insert buffer c
                          _ ->
                            buffer
                        {-case event of
                          Just (EventSpecialKey KeyRightArrow) ->
                            forward buffer
                          Just (EventSpecialKey KeyLeftArrow) ->
                            backward buffer
                          Just (EventSpecialKey KeyDeleteCharacter) ->
                            remove buffer
                          Just (EventSpecialKey KeyBackspace) ->
                            backward buffer
                            >>= remove
                          Just (EventCharacter c) ->
                            let
                              o = (offset buffer) + 1
                            in
                              insert buffer c
                              >>= Just . renderString
                              >>= either
                                  (const Nothing)
                                  (Just . Buffer . fromList . focusFirst . toTagged)
                                  . runParser program () ""
                              >>= (foldl1 (\a b -> \x -> a x >>= b)
                                   $ take o $ repeat forward
                                  )
                          _ ->
                            Just buffer-}
                  {-case buffer' of
                    Just b -> do
                      updateWindow w $ do
                        moveCursor 0 0
                        renderBuffer colorIDs b
                        moveCursor 20 0
                        printDebug b
                      render
                      continue b
                    Nothing -> do
                      continue buffer-}

                  updateWindow w $ do
                    moveCursor 0 0
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
              getEvent w Nothing >>= loop w colorIDs tagged'
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
