module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Monomer
import Monomer.Graph

import Model.AppModel

data AppEvent
    = AppInit
    | AppResetGraph
    | AppAddPoint (Double, Double)
    | AppRemovePoints
    | AppRemoveLast
    | AppPointChange Int (Double, Double)
    deriving (Eq, Show)

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> []
    AppResetGraph -> [Message "mainGraph" GraphReset]
    AppAddPoint p -> [Model $ model & dataPoints %~ (p:)]
    AppRemovePoints -> [Model $ model & dataPoints .~ []]
    AppRemoveLast -> [Model $ model & dataPoints %~ drop 1]
    AppPointChange i p -> [Model $ model & dataPoints . ix i .~ p]
