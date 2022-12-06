{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Monad (unless)
import Data.List.NonEmpty qualified as NE
import Data.List.Ordered
import Data.Time
  ( Day (..),
    LocalTime (..),
    TimeOfDay,
    addLocalTime,
  )
import EventInfo
import System.Exit (exitFailure)
import Test.QuickCheck
  ( Arbitrary,
    Gen,
    arbitrary,
    chooseInt,
    listOf,
    oneof,
    quickCheckResult,
    shrink,
  )
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Test.QuickCheck.Test (isSuccess)
import UI (splitEventsToColumns, toUIEventInfo)
import Prelude

main :: IO ()
main = do
  let tests =
        [ quickCheckResult prop_ui_event_info_list_sorted,
          quickCheckResult prop_column_no_overlap
        ]
  success <- fmap (all isSuccess) . sequence $ tests
  unless success exitFailure

randomNormalEventOn :: Day -> Gen EventInfo
randomNormalEventOn day = do
  uid <- arbitrary
  title <- arbitrary
  description <- arbitrary
  timeOfDay <- (arbitrary :: Gen TimeOfDay)
  before <- positiveSeconds
  after <- positiveSeconds
  let mid = LocalTime day timeOfDay
      start = addLocalTime (fromIntegral (negate before)) mid
      end = addLocalTime (fromIntegral after) mid
  return
    EventInfo
      { eiUID = uid,
        eiDuration = (start, end),
        eiAllDay = False,
        eiTitle = title,
        eiDescription = description
      }
  where
    positiveSeconds =
      oneof
        [ chooseInt (0, 3600 - 1), -- 0 ~ 1 hour
          chooseInt (3600, 24 * 3600 - 1), -- 1 hour ~ 1 day
          chooseInt (24 * 3600, 100 * 24 * 3600) -- 1 ~ 100 days
        ]

data UIEventInfoList = UIEventInfoList [UIEventInfo]
  deriving (Show)

instance Arbitrary UIEventInfoList where
  arbitrary = do
    day <- (arbitrary :: Gen Day)
    events <- listOf $ randomNormalEventOn day
    let sortedEvents = sortOn eventSortKey events
        uiEvents = map (toUIEventInfo day) sortedEvents
    return $ UIEventInfoList uiEvents
  shrink (UIEventInfoList events) =
    map UIEventInfoList $ dropOne [] events
    where
      dropOne :: [a] -> [a] -> [[a]]
      dropOne _ [] = []
      dropOne prev (x : xs) = (prev ++ xs) : dropOne (prev ++ [x]) xs

prop_ui_event_info_list_sorted :: UIEventInfoList -> Bool
prop_ui_event_info_list_sorted (UIEventInfoList events) =
  isSorted $ map (eventSortKey . uiEventInfo) events

prop_column_no_overlap :: UIEventInfoList -> Bool
prop_column_no_overlap (UIEventInfoList events) = all noOverlap $ splitEventsToColumns events
  where
    adjacentPairs column = zip (NE.toList column) (NE.tail column)
    noOverlap column = all validPair $ adjacentPairs column
    validPair (e1, e2) = end1 <= start2
      where
        (_, end1) = uiLogicalDuration e1
        (start2, _) = uiLogicalDuration e2
