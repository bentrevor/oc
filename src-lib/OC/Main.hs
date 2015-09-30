module OC.Main where

import System.Directory
import OC.OrgDay

libMain :: IO ()
libMain = do
  copyFile "/Users/ben/org/calendar.org" "/Users/ben/org/yesterday_calendar.org"
  file <- readFile "/Users/ben/org/yesterday_calendar.org"
  appendFile "/Users/ben/org/calendar.org" $ nextDayStr $ getLastDay file
