module Config
  ( defaultEventDuration,
    logicalTimeResolution,
    minLogicalDuration,
    timelineTopBottomPadding,
  )
where

import Data.Time (NominalDiffTime, secondsToNominalDiffTime)

-- Duration of an event with unspecified duration
defaultEventDuration :: NominalDiffTime
defaultEventDuration = secondsToNominalDiffTime $ 60 * 60 -- 1 hour

-- The unit of time to align events to. To display events we split an event
-- into n logicalTimeResolution lines (doing rounding when necessary).
logicalTimeResolution :: NominalDiffTime
logicalTimeResolution = secondsToNominalDiffTime $ 15 * 60 -- 15 minutes

-- This is the shortest duration we will show in the TUI. An event less than
-- minLogicalDuration long will be displayed as minLogicalDuration lines.
minLogicalDuration :: Int
minLogicalDuration = 3

-- Lines of space to keep before the first event and after the last event
timelineTopBottomPadding :: Int
timelineTopBottomPadding = 2