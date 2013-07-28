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
import Control.Monad.Trans (liftIO)
import Data.List (last)
import Graphics.UI.Gtk
import System.IO (Handle,stdin,hGetLine,hReady)
import System.Posix.IO (stdInput)
import Text.Regex.Posix ((=~))


-- Tweakables
tickerFont = "courier bold 12"
tickerLen  = 60


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


-- Create the label widget
makeLabel :: IO Label
makeLabel = do
   label <- labelNew (Just "")
   miscSetAlignment label 1 0
   font <- fontDescriptionFromString tickerFont
   widgetModifyFont label (Just font)
   return label


-- Ticker state
data Ticker = Ticker Handle Label [String]


-- attach stdin to the label
connectStdinTo :: Label -> IO ()
connectStdinTo label = do
   m <- newMVar $ Ticker stdin label []
   inputAdd (fromEnum stdInput) [IOIn] priorityDefault (readAll m)
   return ()
 

-- drain events from stdin, then update label
readAll :: MVar Ticker -> IO Bool
readAll m = do
   more <- readOne m
   if more
     then readAll m
     else updateLabel m >> return True


-- read and save one line from stdin, returning True if more lines available
readOne :: MVar Ticker -> IO Bool
readOne m = do
   (Ticker h l ss) <- takeMVar m
   s <- hGetLine h
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
      strings  = takeWhile short $ scanl1 join merged
      short x  = length x < tickerLen
      join a b = b ++ " " ++ a
      merged   = if repeat s && repeat t
                   then (s:u)
                   else ss
      repeat x = x =~ "^\\+[0-9]"
      list     = take (length strings) merged

