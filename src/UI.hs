module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Monomer
import Monomer.Graph

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 64]
        [ graphWithData_ points
            [ lockX_ $ model ^. xLock
            , lockY_ $ model ^. yLock
            , onRightClick AppAddPoint
            ] `nodeKey` "mainGraph"
        , separatorLine
        , vstack_ [childSpacing_ 16]
            [ button "Reset" AppResetGraph
            , hgrid_ [childSpacing_ 64]
                [ labeledCheckbox "Lock X" xLock
                , labeledCheckbox "Lock Y" yLock
                ]
            , separatorLine
            , button "Remove points" AppRemovePoints
            , separatorLine
            , toggleButton "Linear" showLinear
            , toggleButton "Quadratic" showQuadratic
            , toggleButton "Cubic" showCubic
            , toggleButton "Power" showPower
            , toggleButton "Exponential" showExponential
            , toggleButton "Logarithmic" showLogarithmic
            ]
        ] `styleBasic` [padding 16]
    points = linear:quadratic:cubic:power:exponential:logarithmic:
        [
            [ graphPoints ps
            , graphColor black
            , graphSeparate
            , graphOnChange AppPointChange
            ]
        ]
    linear = if model ^. showLinear && (not $ null linearF)
        then
            [ graphPoints $ (fromJust linearF) <$> xs
            , graphColor red
            ]
        else []
    quadratic = if model ^. showQuadratic && (not $ null quadF)
        then
            [ graphPoints $ (fromJust quadF) <$> xs
            , graphColor orange
            ]
        else []
    cubic = if model ^. showCubic && (not $ null cubicF)
        then
            [ graphPoints $ (fromJust cubicF) <$> xs
            , graphColor green
            ]
        else []
    power = if model ^. showPower && (not $ null powerF)
        then
            [ graphPoints $ (fromJust powerF) <$> xs
            , graphColor blue
            ]
        else []
    exponential = if model ^. showExponential && (not $ null expF)
        then
            [ graphPoints $ (fromJust expF) <$> xs
            , graphColor violet
            ]
        else []
    logarithmic = if model ^. showLogarithmic && (not $ null logF)
        then
            [ graphPoints $ (fromJust logF) <$> xs
            , graphColor brown
            ]
        else []
    linearF = f <$> makeLinear ps
    quadF = f <$> makeQuadratic ps
    cubicF = f <$> makeCubic ps
    powerF = f <$> makePower ps
    expF = f <$> makeExponential ps
    logF = f <$> makeLogarithmic ps
    f q x = (x, q x)
    ps = model ^. dataPoints
    xs = [-20, (-19.98)..20]
