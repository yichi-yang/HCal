{-# LANGUAGE ImportQualifiedPost #-}

module DateTimeUtil
  ( getStartDateTime,
    getEndDateTime,
    happensOn,
    toLogicalTime,
    getLogicalDuration,
    getDayDurationDescription,
    getDateTimeDurationDescription,
  )
where

import Config (logicalDurationResolution, minLogicalDuration)
import Data.Fixed (Fixed (MkFixed))
import Data.Ord (clamp)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Time (Day, NominalDiffTime, defaultTimeLocale, formatTime, nominalDay, secondsToNominalDiffTime)
import Data.Time.LocalTime (LocalTime (..), TimeZone, addLocalTime, diffLocalTime, midnight, utcToLocalTime)
import Data.Time.Zones qualified as TZ
import Data.Time.Zones.All qualified as TZ
import Text.ICalendar.Types qualified as C

class ToLocalTime a where
  toLocalTime :: a -> TimeZone -> Maybe LocalTime

instance ToLocalTime C.DateTime where
  toLocalTime (C.FloatingDateTime localTime) _ = return localTime
  toLocalTime (C.UTCDateTime utcTime) timeZone = return $ utcToLocalTime timeZone utcTime
  toLocalTime (C.ZonedDateTime time fromTimeZoneText) timeZone = do
    tzLabel <- TZ.tzByName $ encodeUtf8 $ toStrict fromTimeZoneText
    let utcTime = TZ.localTimeToUTCTZ tzLabel time
    return $ utcToLocalTime timeZone utcTime

instance ToLocalTime C.Date where
  toLocalTime (C.Date day) _ = return $ LocalTime day midnight

getStartDateTime :: TimeZone -> C.DTStart -> Maybe LocalTime
getStartDateTime timeZone (C.DTStartDateTime dateTime _) = toLocalTime dateTime timeZone
getStartDateTime timeZone (C.DTStartDate day _) = toLocalTime day timeZone

getEndDateTime :: TimeZone -> LocalTime -> Either C.DTEnd C.DurationProp -> Maybe LocalTime
getEndDateTime timeZone _ (Left (C.DTEndDateTime dateTime _)) = toLocalTime dateTime timeZone
getEndDateTime timeZone _ (Left (C.DTEndDate day _)) = toLocalTime day timeZone
getEndDateTime _ localTimeStart (Right (C.DurationProp duration _)) = return $ addLocalTime diff localTimeStart
  where
    diff = toNominalDiffTime duration

-- Helper to turn x hour, y minutes, z seconds, etc. into seconds
toNominalDiffTimeHelper :: C.Sign -> [(Int, Integer)] -> NominalDiffTime
toNominalDiffTimeHelper s l = secondsToNominalDiffTime $ MkFixed $ sign * foldl multSum 0 l
  where
    multSum acc (count, ratio) = (acc + toInteger count) * ratio
    sign = case s of
      C.Positive -> 1
      C.Negative -> -1

toNominalDiffTime :: C.Duration -> NominalDiffTime
toNominalDiffTime (C.DurationDate durSign durDay durHour durMinute durSecond) =
  toNominalDiffTimeHelper durSign [(durDay, 24), (durHour, 60), (durMinute, 60), (durSecond, 1)]
toNominalDiffTime (C.DurationTime durSign durHour durMinute durSecond) =
  toNominalDiffTimeHelper durSign [(durHour, 60), (durMinute, 60), (durSecond, 1)]
toNominalDiffTime (C.DurationWeek durSign durWeek) =
  toNominalDiffTimeHelper durSign [(durWeek, 7 * 24 * 60 * 60)]

happensOn :: Day -> (LocalTime, LocalTime) -> Bool
happensOn day (start, end) = not $ end <= dayStart || start >= dayEnd
  where
    dayStart = LocalTime day midnight
    dayEnd = addLocalTime nominalDay dayStart

toLogicalTime :: Day -> LocalTime -> Int
toLogicalTime day time = round $ clampedDiff / logicalDurationResolution
  where
    dayStart = LocalTime day midnight
    diff = diffLocalTime time dayStart
    clampedDiff = clamp (0, nominalDay) diff

getLogicalDuration :: Day -> (LocalTime, LocalTime) -> (Int, Int)
getLogicalDuration day (start, end) =
  (logicalStart, logicalEnd)
  where
    logicalStart = toLogicalTime day start
    logicalEnd = max (logicalStart + minLogicalDuration) $ toLogicalTime day end

getDayDurationDescription :: Day -> (LocalTime, LocalTime) -> String
getDayDurationDescription _ (start, end)
  | startDay == endDay = formatTime defaultTimeLocale "%D" startDay
  | otherwise = formatDay startDay ++ " - " ++ formatDay endDay
  where
    formatDay = formatTime defaultTimeLocale "%D"
    startDay = localDay start
    endDay = if localTimeOfDay end > midnight then localDay end else pred $ localDay end

getDateTimeDurationDescription :: Day -> (LocalTime, LocalTime) -> String
getDateTimeDurationDescription activeDay (start, end) = formatDateTime start ++ " - " ++ formatDateTime end
  where
    formatDateTime localTime
      | localDay localTime == activeDay = formatTime defaultTimeLocale "%R" localTime
      | otherwise = formatTime defaultTimeLocale "%D %R" localTime