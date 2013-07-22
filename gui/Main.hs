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

import System.Posix.Types
import System.Posix.IO
import Control.Monad.Trans(liftIO)
import Graphics.UI.Gtk
import Graphics.Rendering.Pango.Font
import qualified System.Glib.MainLoop as M


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
   h <- M.inputAdd (fdFD fd) [IOIn] priorityDefault (readMore fd label)

   mainGUI

fdFD :: Fd -> Int
fdFD (Fd fd) = fromEnum fd

readMore :: Fd -> Label -> IO Bool
readMore fd l = do
   s1 <- labelGetText l
   (s2,n) <- fdRead fd 100
   let x = s1 ++ s2
       m = length x
   labelSetText l $ drop (m - maxChars) x
   widgetQueueDraw l
   return True

