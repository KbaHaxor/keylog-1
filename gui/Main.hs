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
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import Data.List (init,last)
import GHC.IO.Handle (Handle)
import Graphics.UI.Gtk
import System.IO (hGetLine,hReady)
import System.Posix.IO (openFd,OpenMode(ReadWrite),OpenFileFlags(..),fdToHandle)
import System.Posix.Types (Fd(..))
import Text.Regex.Posix ((=~))


maxChars = 40

main :: IO ()
main = do
   initGUI

   window <- windowNew
   window `on` deleteEvent $ liftIO mainQuit >> return False

   vbox <- vBoxNew False 0
   containerAdd window vbox

   font <- fontDescriptionFromString "courier bold 12"

   label <- labelNew (Just "ticker stream...")
   miscSetAlignment label 1 0
   widgetModifyFont label (Just font)
   labelSetSingleLineMode label True
   labelSetMaxWidthChars label maxChars
   labelSetJustify label JustifyRight
   labelSetLineWrap label False
   boxPackStart vbox label PackGrow 0

   widgetShowAll window

   fd <- openFd "../key_stream" ReadWrite Nothing (OpenFileFlags False False False True False)
   h  <- fdToHandle fd
   i  <- inputAdd (fdFD fd) [IOIn] priorityDefault (readMore h label)

   mainGUI

fdFD :: Fd -> Int
fdFD (Fd fd) = fromEnum fd

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

readMore :: Handle -> Label -> IO Bool
readMore h l = do
   s1 <- labelGetText l
   s2 <- hGetLine h
   ss <- hReady h
   labelSetText l $ newLabel s1 s2
   if ss
     then readMore h l
     else widgetQueueDraw l >> return True

