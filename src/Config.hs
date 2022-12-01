module Config
  ( defaultEventDuration,
    logicalDurationResolution,
    minLogicalDuration,
  )
where

import Data.Time (NominalDiffTime, secondsToNominalDiffTime)

-- Duration of an event with unspecified duration
defaultEventDuration :: NominalDiffTime
defaultEventDuration = secondsToNominalDiffTime $ 60 * 60 -- 1 hour

-- The unit of duration to align events to. To display events we split an event
-- into n logicalDurationResolution lines (doing rounding when necessary).
logicalDurationResolution :: NominalDiffTime
logicalDurationResolution = secondsToNominalDiffTime $ 15 * 60 -- 15 minutes

-- This is the shortest duration we will show in the TUI. An event less than
-- minLogicalDuration long will be displayed as minLogicalDuration lines.
minLogicalDuration :: Int
minLogicalDuration = 3
