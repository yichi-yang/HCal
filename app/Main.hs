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
import UI (St (..), app)

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
              St
                { _stRawCalendars = parsedCalendars,
                  _stTimeZone = currTimeZone,
                  _stActiveDay = localDay localTime,
                  _stCursor = Nothing,
                  _stDescOpen = False,
                  _stInputOpen = False
                }
    _ -> do
      putStrLn "Usage: hcal example.ics"