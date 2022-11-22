module Lib
    ( someFunc
    ) where

import Brick

ui :: Widget ()
ui = str "Hello, world!"

someFunc :: IO ()
someFunc = simpleMain ui
