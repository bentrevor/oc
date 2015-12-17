module OC.Main where

import System.Directory
import OC.OrgDay

trackingFilename = "/Users/ben/org/tracking.org"
backupTrackingFilename = "/Users/ben/org/old/backup.tracking.org"

libMain :: IO ()
libMain = do
  copyFile trackingFilename backupTrackingFilename
  file <- readFile backupTrackingFilename
  let todayStr = nextDayStr $ getLastDay file
  appendFile trackingFilename $ "\n" ++ todayStr
