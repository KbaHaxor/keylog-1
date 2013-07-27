-----------------------------------------------------------------------------
-- |
-- Module       : Main.hs
-- Description  : XMonad keylogger gui
-- Copyright    : (c) 2013 Ray Lehtiniemi
-- License      : BSD3-style (see LICENSE)
--
-- Maintainer   : Ray Lehtiniemi <rayl@mail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-----------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.List (init,last)
import Graphics.UI.Gtk
import System.IO (Handle,stdin,hGetLine,hReady)
import System.Posix.IO (stdInput)
import Text.Regex.Posix ((=~))


-- How many characters to display in the ticker window
maxChars :: Int
maxChars = 40


-- Main program
main :: IO ()
main = initGUI >> makeTicker >> mainGUI


-- Create and display a ticker window connected to stdin
makeTicker :: IO ()
makeTicker = do
   window <- windowNew
   window `on` deleteEvent $ liftIO mainQuit >> return False
   vbox <- vBoxNew False 0
   containerAdd window vbox
   font <- fontDescriptionFromString "courier bold 12"
   label <- labelNew (Just "Keylog GUI: ")
   miscSetAlignment label 1 0
   widgetModifyFont label (Just font)
   labelSetSingleLineMode label True
   labelSetMaxWidthChars label maxChars
   labelSetJustify label JustifyRight
   labelSetLineWrap label False
   boxPackStart vbox label PackGrow 0
   widgetShowAll window
   void $ inputAdd (fromEnum stdInput) [IOIn] priorityDefault $ readMore stdin label
   

-- handle one or more lines of event info on stdin
readMore :: Handle -> Label -> IO Bool
readMore h l = do
   s1 <- labelGetText l
   s2 <- hGetLine h
   ss <- hReady h
   labelSetText l $ newLabel s1 s2
   if ss
     then readMore h l
     else widgetQueueDraw l >> return True


-- update label text with new event
newLabel :: String -> String -> String
newLabel cur add = drop excess newText
         where
            excess = length newText - maxChars
            newText = base ++ "  " ++ tail
            base | add == "+ 1"   = cur
                 | add =~ "^\\+ " = rewind cur
                 | otherwise      = cur
            tail | add =~ "^\\+ " = counter
                 | otherwise      = add
            rewind c = if isSpace $ last c
                       then init . init $ c
                       else rewind $ init c
            counter = "+" ++ (drop 2 add)

