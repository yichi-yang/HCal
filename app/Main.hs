{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Brick.AttrMap
  ( AttrName,
    attrMap,
    attrName,
  )
import Brick.Main qualified as M
import Brick.Types (Location (..), Widget, locationColumnL, locationRowL)
import Brick.Types qualified as T
import Brick.Util (fg)
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core
  ( relativeTo,
    reportExtent,
    str,
    translateBy,
    txt,
    vBox,
    withDefAttr,
    (<=>)
  )
import Control.Monad (void)
import Data.Default
import Data.Map.Strict (elems)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time (TimeZone, getCurrentTimeZone)
import DateTimeUtil
import Graphics.Vty qualified as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH (makeLenses)
import System.Environment
import Text.ICalendar.Parser
import Text.ICalendar.Types qualified as C
import Data.Text.Lazy (toStrict)
import Brick.Widgets.Border (border)

data St = St
  { _rawCalendars :: [C.VCalendar],
    _timeZone :: TimeZone
  }

makeLenses ''St

data Name
  = MiddleLayerElement
  deriving (Ord, Eq, Show)

drawUi :: St -> [Widget Name]
drawUi st =
  [drawEvents st]

-- arrowLayer :: Widget Name
-- arrowLayer =
--     let msg = "Relatively\n" <>
--               "positioned\n" <>
--               "arrow---->"
--     in relativeTo MiddleLayerElement (Location (-10, -2)) $
--        withDefAttr arrowAttr $
--        str msg

-- middleLayer :: St -> Widget Name
-- middleLayer st =
--     translateBy (st^.middleLayerLocation) $
--     reportExtent MiddleLayerElement $
--     B.border $ str "Middle layer\n(Arrow keys move)"

-- bottomLayer :: St -> Widget Name
-- bottomLayer st =
--     translateBy (st^.bottomLayerLocation) $
-- B.border $ str "Bottom layer\n(Ctrl-arrow keys move)"

drawEvents :: St -> Widget Name
drawEvents st =
  vBox $ map (border . drawEvent) events
  where
    -- rawEvents :: [C.VEvent]
    rawEvents = concatMap (elems . C.vcEvents) (st ^. rawCalendars)
    events = mapMaybe (flip prepareEventInfo (st ^. timeZone)) rawEvents
    drawEvent event = (txt . toStrict . fromMaybe "" . eventTitle) event <=> (str $ show $ fst $ eventDuration event)

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
-- appEvent (T.VtyEvent (V.EvKey V.KDown []))  =
--     middleLayerLocation.locationRowL %= (+ 1)
-- appEvent (T.VtyEvent (V.EvKey V.KUp []))    =
--     middleLayerLocation.locationRowL %= (subtract 1)
-- appEvent (T.VtyEvent (V.EvKey V.KRight [])) =
--     middleLayerLocation.locationColumnL %= (+ 1)
-- appEvent (T.VtyEvent (V.EvKey V.KLeft []))  =
--     middleLayerLocation.locationColumnL %= (subtract 1)

-- appEvent (T.VtyEvent (V.EvKey V.KDown  [V.MCtrl])) =
--     bottomLayerLocation.locationRowL %= (+ 1)
-- appEvent (T.VtyEvent (V.EvKey V.KUp    [V.MCtrl])) =
--     bottomLayerLocation.locationRowL %= (subtract 1)
-- appEvent (T.VtyEvent (V.EvKey V.KRight [V.MCtrl])) =
--     bottomLayerLocation.locationColumnL %= (+ 1)
-- appEvent (T.VtyEvent (V.EvKey V.KLeft  [V.MCtrl])) =
--     bottomLayerLocation.locationColumnL %= (subtract 1)

appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent _ = return ()

arrowAttr :: AttrName
arrowAttr = attrName "attr"

app :: M.App St e Name
app =
  M.App
    { M.appDraw = drawUi,
      M.appStartEvent = return (),
      M.appHandleEvent = appEvent,
      M.appAttrMap = const $ attrMap V.defAttr [(arrowAttr, fg V.cyan)],
      M.appChooseCursor = M.neverShowCursor
    }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      parseResults <- parseICalendarFile def path
      case parseResults of
        Left errorMsg -> do
          putStrLn errorMsg
        Right (parsedCalendars, _) -> do
          currTimeZone <- getCurrentTimeZone
          void $ M.defaultMain app $ St {_rawCalendars = parsedCalendars, _timeZone = currTimeZone}
    _ -> do
      putStrLn "Usage: hcal example.ics"