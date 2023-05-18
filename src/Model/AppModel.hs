{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( Info(..)
    , Menu(..)
    , AppModel(..)
    , xLock
    , yLock
    , dataPoints
    , currentInfo
    , currentMenu
    , showLinear
    , showQuadratic
    , showCubic
    , showPower
    , showExponential
    , showLogarithmic
    , initModel
    ) where

import Control.Lens

data Info
    = ILinear
    | IQuad
    | ICubic
    | IPower
    | IExp
    | ILog
    deriving (Eq, Show)

data Menu
    = MPoints
    | MApprox
    deriving (Eq, Show)

data AppModel = AppModel
    { _amXLock :: Bool
    , _amYLock :: Bool
    , _amDataPoints :: [(Double, Double)]
    , _amCurrentInfo :: Info
    , _amCurrentMenu :: Menu
    , _amShowLinear :: Bool
    , _amShowQuadratic :: Bool
    , _amShowCubic :: Bool
    , _amShowPower :: Bool
    , _amShowExponential :: Bool
    , _amShowLogarithmic :: Bool
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = AppModel
    { _amXLock = False
    , _amYLock = False
    , _amDataPoints = []
    , _amCurrentInfo = ILinear
    , _amCurrentMenu = MPoints
    , _amShowLinear = False
    , _amShowQuadratic = False
    , _amShowCubic = False
    , _amShowPower = False
    , _amShowExponential = False
    , _amShowLogarithmic = False
    }
