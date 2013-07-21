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

import Graphics.UI.Gtk
import Control.Monad.Trans(liftIO)

main :: IO ()
main = do
   initGUI

   window <- windowNew
   window `on` deleteEvent $ liftIO mainQuit >> return False

   vbox <- vBoxNew False 0
   containerAdd window vbox

   label <- labelNew (Just "ticker stream...")
   miscSetAlignment label 1 0
   boxPackStart vbox label PackGrow 0
   labelSetJustify label JustifyRight
   labelSetLineWrap label False


   widgetShowAll window
   mainGUI

