{-# LANGUAGE ImportQualifiedPost #-}

module EventInfo
  ( 
    prepareEventInfo,
    EventInfo (..),
    UIEventInfo (..),
  )
where

import Data.Text.Lazy (Text)
import Data.Time (LocalTime (..), addLocalTime)
import Data.Time.LocalTime (TimeZone)
import DateTimeUtil (getEndDateTime, getStartDateTime)
import Text.ICalendar.Types qualified as C
import Config (defaultEventDuration)

-- Things about an event that we cares about
data EventInfo = EventInfo
  { eiUID :: Text,
    eiDuration :: (LocalTime, LocalTime),
    eiAllDay :: Bool,
    eiTitle :: Maybe Text,
    eiDescription :: Maybe Text
  }
  deriving (Show, Eq)

prepareEventInfo :: C.VEvent -> TimeZone -> Maybe EventInfo
prepareEventInfo event timeZone = do
  start <- C.veDTStart event
  localTimeStart <- getStartDateTime timeZone start

  let title = C.summaryValue <$> C.veSummary event
      description = C.descriptionValue <$> C.veDescription event
      uid = C.uidValue $ C.veUID event
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
      { eiUID = uid,
        eiDuration = (localTimeStart, localTimeEnd),
        eiAllDay = allDay,
        eiTitle = title,
        eiDescription = description
      }

-- Information we need to render an event
data UIEventInfo = UIEventInfo
  { uiEventInfo :: EventInfo,
    -- logical (start, end), i.e. the starting & ending line numbers
    uiLogicalDuration :: (Int, Int),
    -- is the event selected (highlighted in the UI)
    uiSelected :: Bool,
    -- duration description to show on the UI
    uiDurationDescription :: String
  }
  deriving (Show, Eq)
