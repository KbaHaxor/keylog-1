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
import Control.Concurrent.MVar (MVar,newMVar,takeMVar,putMVar)
import Control.Monad.Loops (iterateUntil)
import Control.Monad.Trans (liftIO)
import Data.List (last,takeWhile)
import Graphics.UI.Gtk
import System.IO (Handle,stdin,hGetLine,hReady)
import System.Posix.IO (stdInput)
import Text.Regex.Posix ((=~))


-- Tweakables
tickerFont = "courier bold 10"
tickerLen  = 70


-- Entry point
main = initGUI >> makeTicker >> mainGUI


-- Create and display a ticker window displaying event stream on stdin
makeTicker :: IO ()
makeTicker = do
   window <- windowNew
   window `on` deleteEvent $ liftIO mainQuit >> return False
   vbox <- vBoxNew True 0
   containerAdd window vbox
   label <- makeLabel
   boxPackStart vbox label PackGrow 0
   connectStdinTo label
   widgetShowAll window
   positionWindow window


-- Create the label widget
makeLabel :: IO Label
makeLabel = do
   label <- labelNew (Just "")
   miscSetAlignment label 1 0
   font <- fontDescriptionFromString tickerFont
   widgetModifyFont label (Just font)
   labelSetWidthChars label tickerLen
   return label


-- position window at bottom center of 1080p display
positionWindow :: Window -> IO ()
positionWindow window = do
   (ww,wh) <- windowGetSize window
   windowMove window ((1920 - ww) `div` 2) (1080 - wh)


-- Ticker state
data Ticker = Ticker Handle Label [String]


-- attach stdin to the label
connectStdinTo :: Label -> IO HandlerId
connectStdinTo label = do
   m <- newMVar $ Ticker stdin label []
   inputAdd (fromEnum stdInput) [IOIn] priorityDefault (readAll m)
 

-- drain events from stdin, then update label
readAll :: MVar Ticker -> IO Bool
readAll m = iterateUntil not (readOne m) >> updateLabel m >> return True


-- read and save one line from stdin, returning True if more lines available
readOne :: MVar Ticker -> IO Bool
readOne m = do
   (Ticker h l ss) <- takeMVar m
   k <- hGetLine h
   let s = takeWhile (/= '\t') k
   putMVar m $ Ticker h l (s:ss)
   hReady h


-- update label text and discard old events
updateLabel :: MVar Ticker -> IO ()
updateLabel m = do
   (Ticker h l ss) <- takeMVar m
   let (t,ss') = processEvents ss
   putMVar m $ Ticker h l ss'
   labelSetText l t
   widgetQueueDraw l


-- given a list of events, most recent first, merge typematic repeat
-- events, then join the merged event list into a string not exceeding
-- tickerLen in length. discard older events not included in the string
-- and return the string along with the trimmed event list
processEvents :: [String] -> (String,[String])
processEvents []         = ("", [])
processEvents [s]        = (s, [s])
processEvents ss@(s:t:u) = (string,list)
   where
      string   = last strings
      list     = take (length strings) merged

      strings  = takeWhile fits joined
                   where
                     fits x = length x <= tickerLen
                     joined = scanl1 join merged
                     join a b = b ++ sep ++ a
                       where
                         sep | repeat a   = ""
                             | multi a    = ""
                             | otherwise  = " "

      merged   = if both repeat || both multi
                   then (s:u)
                   else ss
                 where
                   both f = f s && f t

      repeat x = x =~ "^\\+[0-9]"
      multi x  = x =~ "^\\*[0-9]"
