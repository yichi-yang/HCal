{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Brick.Main qualified as BM
import Control.Monad (void)
import Data.Default (Default (def))
import Data.Time
  ( LocalTime (localDay),
    getCurrentTime,
    getCurrentTimeZone,
    utcToLocalTime,
  )
import System.Environment (getArgs)
import Text.ICalendar.Parser (parseICalendarFile)
import UI (app, initSt)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      parseResults <- parseICalendarFile def path
      case parseResults of
        Left errorMsg -> do
          putStrLn errorMsg
        Right (parsedCalendars, _) -> do
          currTimeZone <- getCurrentTimeZone
          utcNow <- getCurrentTime
          let localTime = utcToLocalTime currTimeZone utcNow
          void $
            BM.defaultMain app $
              initSt parsedCalendars currTimeZone (localDay localTime)
    _ -> do
      putStrLn "Usage: hcal example.ics"