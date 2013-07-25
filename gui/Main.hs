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

import System.IO (hGetLine,hReady)
import GHC.IO.Handle (Handle)
import System.Posix.Types (Fd(..))
import System.Posix.IO (openFd,OpenMode(ReadWrite),OpenFileFlags(..),fdToHandle)
import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk
import Graphics.Rendering.Pango.Font ()
import qualified System.Glib.MainLoop as M (inputAdd)


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
   i  <- M.inputAdd (fdFD fd) [IOIn] priorityDefault (readMore h label)

   mainGUI

fdFD :: Fd -> Int
fdFD (Fd fd) = fromEnum fd

readMore :: Handle -> Label -> IO Bool
readMore h l = do
   s1 <- labelGetText l
   s2 <- hGetLine h
   let x = s1 ++ "  " ++ s2
       m = length x
   labelSetText l $ drop (m - maxChars) x
   widgetQueueDraw l
   more <- hReady h
   if more
     then readMore h l
     else return True

