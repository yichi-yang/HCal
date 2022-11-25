{-# LANGUAGE ImportQualifiedPost #-}

module DateTimeUtil
  ( prepareEventInfo,
    EventInfo (..),
  )
where

import Data.Fixed (Fixed (MkFixed))
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (Text, toStrict)
import Data.Time (LocalTime (LocalTime), NominalDiffTime, addLocalTime, midnight, secondsToNominalDiffTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import Data.Time.Zones qualified as TZ
import Data.Time.Zones.All qualified as TZ
import Text.ICalendar.Types qualified as C
import Config(defaultEventDuration)

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

-- getEndDateTime C.DTStartDate{C.dtStartDateValue=day} timeZone              = toLocalTime day timeZone

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

data EventInfo = EventInfo
  { eventDuration :: (LocalTime, LocalTime),
    eventAllDay :: Bool,
    eventTitle :: Maybe Text,
    eventDescription :: Maybe Text
  }
  deriving (Show, Eq)

prepareEventInfo :: C.VEvent -> TimeZone -> Maybe EventInfo
prepareEventInfo event timeZone = do
  start <- C.veDTStart event
  localTimeStart <- getStartDateTime timeZone start

  let title = C.summaryValue <$> C.veSummary event
      description = C.descriptionValue <$> C.veDescription event
  localTimeEnd <- case C.veDTEndDuration event of
        -- if end date time is not provided
        -- assume the event has defaultEventDuration
        Nothing -> return $ addLocalTime defaultEventDuration localTimeStart
        Just end -> getEndDateTime timeZone localTimeStart end

  let allDay = case start of
        C.DTStartDateTime {} -> False
        C.DTStartDate {} -> True
  return
    EventInfo
      { eventDuration = (localTimeStart, localTimeEnd),
        eventAllDay = allDay,
        eventTitle = title,
        eventDescription = description
      }