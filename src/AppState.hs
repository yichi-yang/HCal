module AppState
  ( AppState(..),
  )
where

import Text.ICalendar.Types

data AppState = AppState
  { _cals :: [VCalendar]
  }
  deriving (Show, Eq)