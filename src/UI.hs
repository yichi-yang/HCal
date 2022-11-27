{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module UI (app, St (..)) where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import Brick.AttrMap
  ( AttrMap,
    AttrName,
    attrMap,
    attrName,
  )
import Brick.BorderMap (Edges (..))
import Brick.Main qualified as BM
import Brick.Types qualified as BT
import Brick.Util qualified as BU
import Brick.Widgets.Border (hBorder, hBorderWithLabel, joinableBorder, vBorder)
import Brick.Widgets.Core
  ( Padding (..),
    emptyWidget,
    fill,
    hBox,
    joinBorders,
    padBottom,
    padLeft,
    padRight,
    str,
    txt,
    vBox,
    vLimit,
    viewport,
    visible,
    withAttr,
    withVScrollBars,
    (<+>),
    (<=>),
  )
import Config (logicalDurationResolution)
import Control.Lens.At (ix)
import Control.Monad.State.Lazy
  ( MonadState (get, put),
    State,
    evalState,
    execState,
    modify,
  )
import Data.Function (on)
import Data.List (minimumBy, partition, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (elems)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text.Lazy (toStrict)
import Data.Time
  ( Day,
    LocalTime,
    TimeZone,
    defaultTimeLocale,
    formatTime,
    nominalDay,
    timeToDaysAndTimeOfDay,
  )
import DateTimeUtil
  ( getDateTimeDurationDescription,
    getDayDurationDescription,
    getLogicalDuration,
    happensOn,
  )
import EventInfo
  ( EventInfo (..),
    UIEventInfo (..),
    prepareEventInfo,
  )
import Graphics.Vty qualified as V
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl ((%=))
import Lens.Micro.TH (makeLenses)
import ListCursor (ListCursor (..), mapToList, next, prev, tryApply)
import Text.ICalendar.Types qualified as C

-- State of our app
data St = St
  { _stRawCalendars :: [C.VCalendar],
    _stTimeZone :: TimeZone,
    _stActiveDay :: Day,
    _stCursor :: Maybe (ListCursor UIEventInfo)
  }

makeLenses ''St

data Name
  = EventVP
  deriving (Ord, Eq, Show)

drawUi :: St -> [BT.Widget Name]
drawUi st =
  [drawEventViewport st]

drawEventViewport :: St -> BT.Widget Name
drawEventViewport st = drawHelper $ st ^. stCursor
  where
    label = str $ formatTime defaultTimeLocale "%D" (st ^. stActiveDay)
    banner = withAttr bannerAttr $ hBorderWithLabel label
    drawHelper :: Maybe (ListCursor UIEventInfo) -> BT.Widget Name
    drawHelper Nothing = banner <=> drawNormalEvents []
    drawHelper (Just cursor) =
      withVScrollBars BT.OnRight $ viewport EventVP BT.Vertical viewPortContent
      where
        events = mapToList updateUiSelected cursor
        updateUiSelected selected eventInfo = eventInfo {uiSelected = selected}
        (allDayEvents, normalEvents) = partition (eiAllDay . uiEventInfo) events
        viewPortContent =
          joinBorders $
            drawAllDayEvents allDayEvents
              <=> banner
              <=> drawNormalEvents normalEvents

getWithAttrIfSelected :: Bool -> (BT.Widget Name -> BT.Widget Name, BT.Widget Name -> BT.Widget Name)
getWithAttrIfSelected selected =
  if selected
    then (visible, withAttr selectedAttr)
    else (id, id)

joinableHBorder :: BT.Widget Name
joinableHBorder =
  joinableBorder (Edges False False False True)
    <+> hBorder
    <+> joinableBorder (Edges False False True False)

drawAllDayEvents :: [UIEventInfo] -> BT.Widget Name
drawAllDayEvents events = padLeft (Pad 6) $ vBox (map drawEventBody events) <=> joinableHBorder
  where
    drawEventBody UIEventInfo {uiEventInfo = eventInfo, uiSelected = selected, uiDurationDescription = durationStr} =
      setVisible $ vLimit 3 $ joinableHBorder <=> (vBorder <+> setSelected (padRight Max $ row1 <=> row2) <+> vBorder)
      where
        EventInfo {eiTitle = title} = eventInfo
        row1 = txt $ toStrict $ fromMaybe "[No Title]" title
        row2 = str durationStr
        (setVisible, setSelected) = getWithAttrIfSelected selected

drawNormalEvents :: [UIEventInfo] -> BT.Widget Name
drawNormalEvents [] = padLeft (Pad 6) $ txt "Your timeline is empty today :)"
drawNormalEvents events = drawTimeline <+> hBox (map (drawEventColumn . NE.toList) columns)
  where
    columns = splitEventsToColumns events
    drawTimeline = padRight (Pad 1) $ vBox $ map str timeOfDayStr
    timeOfDayStr = map (formatTime defaultTimeLocale "%R") timeSteps
    timeSteps = map (snd . timeToDaysAndTimeOfDay) [0, logicalDurationResolution .. nominalDay]

-- Split uiEvents into n non-overlapping columns
splitEventsToColumns :: [UIEventInfo] -> [NE.NonEmpty UIEventInfo]
splitEventsToColumns uiEvents = reverse $ map NE.reverse $ execState (splitHelper uiEvents) []
  where
    getColumnEndTime :: NE.NonEmpty UIEventInfo -> LocalTime
    getColumnEndTime column = snd $ eiDuration $ uiEventInfo $ NE.head column
    splitHelper :: [UIEventInfo] -> State [NE.NonEmpty UIEventInfo] ()
    splitHelper [] = return ()
    splitHelper (event : rest) = do
      -- a list of event existing columns
      columns <- get
      case columns of
        -- if we have no column, just add one
        [] -> do
          put [NE.fromList [event]]
          _ <- splitHelper rest
          return ()
        -- if we have existing columns, check to see if they fit the current event
        _ ->
          do
            let startTime = fst $ eiDuration $ uiEventInfo event
                pairs :: [(Int, NE.NonEmpty UIEventInfo)]
                pairs = zip [0 ..] columns
                -- find the column with earliest ending time
                (bestI, bestColumn) = minimumBy (compare `on` \(i, c) -> (getColumnEndTime c, i)) pairs
                earliestEndTime = getColumnEndTime bestColumn
            if earliestEndTime > startTime
              then -- if it doesn't fit, create a new column
              do
                put $ NE.fromList [event] : columns
                _ <- splitHelper rest
                return ()
              else -- else just append to that column
              do
                put $ columns & ix bestI .~ event NE.<| bestColumn
                _ <- splitHelper rest
                return ()

-- draw a single column of events
drawEventColumn :: [UIEventInfo] -> BT.Widget Name
drawEventColumn events = evalState (drawHelper $ reverse events) Nothing
  where
    getBottomBorder :: Int -> Int -> BT.Widget Name
    getBottomBorder row end
      | row == end = emptyWidget
      | row > end = padBottom (Pad (row - end - 1)) joinableHBorder
      | otherwise = error $ "Events in a column must not overlap -> " ++ show events
    drawHelper :: [UIEventInfo] -> State (Maybe Int) (BT.Widget Name)
    drawHelper [] = do
      maybeRow <- get
      let padding = case maybeRow of
            Nothing -> joinableHBorder
            Just row -> vLimit row $ fill ' '
      return padding
    drawHelper
      ( UIEventInfo
          { uiEventInfo = eventInfo,
            uiLogicalDuration = (start, end),
            uiSelected = selected,
            uiDurationDescription = durationStr
          }
          : rest
        ) = do
        maybeRow <- get
        let EventInfo {eiTitle = title} = eventInfo
            row1 = txt $ toStrict $ fromMaybe "[No Title]" title
            row2 = str durationStr
            contentHeight = end - start - 1
            internalPadding = contentHeight - 2
            content = padBottom (Pad internalPadding) $ padRight Max (row1 <=> row2)
            (setVisible, setSelected) = getWithAttrIfSelected selected
            body =
              setVisible $
                joinableHBorder <=> vLimit contentHeight (vBorder <+> setSelected content <+> vBorder)
            bottomBorder = case maybeRow of
              Nothing -> joinableHBorder
              Just row -> getBottomBorder row end
        put $ Just start
        widgetRest <- drawHelper rest
        return $ widgetRest <=> body <=> bottomBorder

-- Creates a new ListCursor UIEventInfo by selecting all events that
-- overlaps with activateDay and setting the first event as the selected
-- event. If there's no event on activateDay, returns Nothing.
buildListCursor :: Day -> TimeZone -> [C.VEvent] -> Maybe (ListCursor UIEventInfo)
buildListCursor activeDay timeZone rawEvents = case uiEvents of
  (first : rest) -> Just (ListCursor first [] rest)
  _ -> Nothing
  where
    -- filter out all events that happen on activeDay
    events = filter (happensOn activeDay . eiDuration) $ mapMaybe (`prepareEventInfo` timeZone) rawEvents
    -- sort by (is all day, start date time, uid) ascending
    sortedEventInfo = sortOn eventSortKey events
    -- add information needed for drawing events
    uiEvents = map toUIEventInfo sortedEventInfo
    eventSortKey EventInfo {eiDuration = (start, _), eiUID = uid, eiAllDay = allDay} = (not allDay, start, uid)
    toUIEventInfo ei =
      let duration = eiDuration ei
          allDay = eiAllDay ei
          durationDescription =
            if allDay
              then getDayDurationDescription activeDay duration
              else getDateTimeDurationDescription activeDay duration
       in UIEventInfo
            { uiEventInfo = ei,
              uiLogicalDuration = getLogicalDuration activeDay duration,
              uiSelected = False, -- set it to false For now
              uiDurationDescription = durationDescription
            }

-- Initialize the cursor
appStartEvent :: BT.EventM Name St ()
appStartEvent = do
  st <- get
  let activeDay = st ^. stActiveDay
      timeZone = st ^. stTimeZone
      rawEvents = concatMap (elems . C.vcEvents) (st ^. stRawCalendars)
  _ <- put (st & stCursor .~ buildListCursor activeDay timeZone rawEvents)
  return ()

-- Update stActiveDay with f and update the cursor accordingly
updateActiveDay :: (Day -> Day) -> St -> St
updateActiveDay f st = st & stCursor .~ cursor & stActiveDay .~ activeDay
  where
    activeDay = f $ st ^. stActiveDay
    timeZone = st ^. stTimeZone
    rawEvents = concatMap (elems . C.vcEvents) (st ^. stRawCalendars)
    cursor = buildListCursor activeDay timeZone rawEvents

appEvent :: BT.BrickEvent Name e -> BT.EventM Name St ()
appEvent (BT.VtyEvent (V.EvKey V.KDown [])) =
  stCursor %= tryApply next
appEvent (BT.VtyEvent (V.EvKey V.KUp [])) =
  stCursor %= tryApply prev
appEvent (BT.VtyEvent (V.EvKey V.KRight [])) = do
  modify $ updateActiveDay succ
appEvent (BT.VtyEvent (V.EvKey V.KLeft [])) =
  modify $ updateActiveDay pred
appEvent (BT.VtyEvent (V.EvKey V.KEsc [])) = BM.halt
appEvent _ = return ()

selectedAttr :: AttrName
selectedAttr = attrName "selected"

bannerAttr :: AttrName
bannerAttr = attrName "banner"

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (selectedAttr, V.black `BU.on` V.yellow),
      (bannerAttr, V.black `BU.on` V.white)
    ]

app :: BM.App St e Name
app =
  BM.App
    { BM.appDraw = drawUi,
      BM.appStartEvent = appStartEvent,
      BM.appHandleEvent = appEvent,
      BM.appAttrMap = const theMap,
      BM.appChooseCursor = BM.neverShowCursor
    }