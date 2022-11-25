module Config
  ( defaultEventDuration,
    logicalDurationResolution,
  )
where

import Data.Time (NominalDiffTime, secondsToNominalDiffTime)

-- Duration of an event with unspecified duration
defaultEventDuration :: NominalDiffTime
defaultEventDuration = secondsToNominalDiffTime $ 60 * 60 -- 1 hour

-- The unit of duration to align events to.
-- This is the shortest duration we will show in the TUI.
-- To display events we split an event into n logicalDurationResolution
-- chunks (doing rounding when necessary). An event less than
-- logicalDurationResolution long will be displayed as one chunk.
logicalDurationResolution :: NominalDiffTime
logicalDurationResolution = secondsToNominalDiffTime $ 30 * 60 -- 30 minutes