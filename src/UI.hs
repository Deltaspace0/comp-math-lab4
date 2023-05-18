module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import Monomer.Graph

import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = tree where
    tree = hstack_ [childSpacing_ 16]
        [ vstack'
            [ graphWithData_ points
                [ lockX_ $ model ^. xLock
                , lockY_ $ model ^. yLock
                , onRightClick AppAddPoint
                ] `nodeKey` "mainGraph"
            , separatorLine
            , vstack' $ case model ^. currentInfo of
                ILinear ->
                    [ label "Linear approximation:"
                    , label $ ft $ textLinear <$> linearSolution
                    , label $ textStd ps $ (snd .) <$> linearF
                    , label $ "Pearson: " <> (numericToText 5 pn)
                    ] where pn = getPearson ps
                IQuad ->
                    [ label "Quadratic approximation:"
                    , label $ ft $ textQuadratic <$> quadSolution
                    , label $ textStd ps $ (snd .) <$> quadF
                    ]
                ICubic ->
                    [ label "Cubic approximation:"
                    , label $ ft $ textCubic <$> cubicSolution
                    , label $ textStd ps $ (snd .) <$> cubicF
                    ]
                IPower ->
                    [ label "Power approximation:"
                    , label $ ft $ textPower <$> powerSolution
                    , label $ textStd ps $ (snd .) <$> powerF
                    ]
                IExp ->
                    [ label "Exponential approximation:"
                    , label $ ft $ textExponential <$> expSolution
                    , label $ textStd ps $ (snd .) <$> expF
                    ]
                ILog ->
                    [ label "Logarithmic approximation:"
                    , label $ ft $ textLogarithmic <$> logSolution
                    , label $ textStd ps $ (snd .) <$> logF
                    ]
            ]
        , separatorLine
        , vstack'
            [ button "Reset" AppResetGraph
            , hgrid_ [childSpacing_ 64]
                [ labeledCheckbox "Lock X" xLock
                , labeledCheckbox "Lock Y" yLock
                ]
            , separatorLine
            , hgrid'
                [ optionButton "Points" MPoints currentMenu
                , optionButton "Approximations" MApprox currentMenu
                ]
            , separatorLine
            , case model ^. currentMenu of
                MPoints -> vstack'
                    [ label "Add points with right mouse button"
                    , hgrid'
                        [ button "Remove all" AppRemovePoints
                        , button "Remove last" AppRemoveLast
                        ]
                    , vscroll $ vstack' $
                        [ hgrid'
                            [ label "X:"
                            , label "Y:"
                            ]
                        ] <> pointPanels
                    ]
                MApprox -> vscroll $ vstack'
                    [ hgrid'
                        [ toggleButton "Linear" showLinear
                        , optionButton "Info" ILinear currentInfo
                        ]
                    , hgrid'
                        [ toggleButton "Quadratic" showQuadratic
                        , optionButton "Info" IQuad currentInfo
                        ]
                    , hgrid'
                        [ toggleButton "Cubic" showCubic
                        , optionButton "Info" ICubic currentInfo
                        ]
                    , hgrid'
                        [ toggleButton "Power" showPower
                        , optionButton "Info" IPower currentInfo
                        ]
                    , hgrid'
                        [ toggleButton "Exponential" showExponential
                        , optionButton "Info" IExp currentInfo
                        ]
                    , hgrid'
                        [ toggleButton "Logarithmic" showLogarithmic
                        , optionButton "Info" ILog currentInfo
                        ]
                    , separatorLine
                    , label "Best approximation (least SD):"
                    , label bestApproximation
                    ]
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
    linearF = f <$> makeLinear <$> linearSolution
    quadF = f <$> makeQuadratic <$> quadSolution
    cubicF = f <$> makeCubic <$> cubicSolution
    powerF = f <$> makePower <$> powerSolution
    expF = f <$> makeExponential <$> expSolution
    logF = f <$> makeLogarithmic <$> logSolution
    linearSolution = solveLinear ps
    quadSolution = solveQuadratic ps
    cubicSolution = solveCubic ps
    powerSolution = solvePower ps
    expSolution = solveExponential ps
    logSolution = solveLogarithmic ps
    ft = fromMaybe "No solution"
    f q x = (x, q x)
    ps = model ^. dataPoints
    xs = [-20, (-19.98)..20]
    pointPanels = makePointPanel <$> [0..length ps-1]
    makePointPanel i = hgrid'
        [ numericField_ (pointField i . _1) [decimals 3]
        , numericField_ (pointField i . _2) [decimals 3]
        ]
    pointField i = lens getter setter where
        getter = (^?! ix i) . _amDataPoints
        setter = flip $ set $ dataPoints . ix i
    vstack' = vstack_ [childSpacing_ 16]
    hgrid' = hgrid_ [childSpacing_ 16]
    bestApproximation = text where
        text = if null result
            then "???"
            else fst $ fromJust result
        result = foldl1 findBest approximationsWithStd
        findBest Nothing Nothing = Nothing
        findBest Nothing x = x
        findBest x Nothing = x
        findBest a@(Just (_, s1)) b@(Just (_, s2)) = res where
            res = if abs s1 < abs s2 || isNaN s2 then a else b
    approximationsWithStd =
        [ ((,) "Linear" . getDeviation ps . (snd .)) <$> linearF
        , ((,) "Quadratic" . getDeviation ps . (snd .)) <$> quadF
        , ((,) "Cubic" . getDeviation ps . (snd .)) <$> cubicF
        , ((,) "Power" . getDeviation ps . (snd .)) <$> powerF
        , ((,) "Exponential" . getDeviation ps . (snd .)) <$> expF
        , ((,) "Logarithmic" . getDeviation ps . (snd .)) <$> logF
        ]

textStd :: [(Double, Double)] -> Maybe (Double -> Double) -> Text
textStd _ Nothing = "???"
textStd ps (Just f) = "Standard deviation: " <> d where
    d = numericToText 5 $ getDeviation ps f

textLinear :: (Double, Double) -> Text
textLinear (b, a) = "f(x) = " <> a' <> "x " <> b' where
    a' = numericToText 5 a
    b' = formatNumber b

textQuadratic :: (Double, Double, Double) -> Text
textQuadratic (c, b, a) = "f(x) = " <> ax2 <> bx <> c' where
    ax2 = (numericToText 5 a) <> "x^2 "
    bx = (formatNumber b) <> "x "
    c' = formatNumber c

textCubic :: (Double, Double, Double, Double) -> Text
textCubic (d, c, b, a) = "f(x) = " <> ax3 <> bx2 <> cx <> d' where
    ax3 = (numericToText 5 a) <> "x^3 "
    bx2 = (formatNumber b) <> "x^2 "
    cx = (formatNumber c) <> "x "
    d' = formatNumber d

textPower :: (Double, Double) -> Text
textPower (a, b) = "f(x) = " <> a' <> "x^" <> b' where
    a' = numericToText 5 a
    b' = numericToText 5 b

textExponential :: (Double, Double) -> Text
textExponential (a, b) = "f(x) = " <> a' <> eb where
    a' = numericToText 5 a
    eb = "e^(" <> (numericToText 5 b) <> "x)"

textLogarithmic :: (Double, Double) -> Text
textLogarithmic (a, b) = "f(x) = " <> a' <> " " <> bln where
    a' = numericToText 5 a
    bln = (formatNumber b) <> "lnx"

formatNumber :: Double -> Text
formatNumber x = sign <> (numericToText 5 $ abs x) where
    sign = if x >= 0 then "+ " else "- "
