module AppState
  ( AppState(..),
    test123,
  )
where

import Text.ICalendar.Types

data AppState = AppState
  { _cals :: [VCalendar]
  }
  deriving (Show, Eq)

test123 :: Integer -> Integer -> Integer
test123 = (+)