module Model.Method
    ( makeLinear
    , makeQuadratic
    , makeCubic
    , makePower
    , makeExponential
    , makeLogarithmic
    , solveLinear
    , solveQuadratic
    , solveCubic
    , solvePower
    , solveExponential
    , solveLogarithmic
    , getDeviation
    , getPearson
    ) where

import Data.Matrix hiding (toList)
import Data.Vector (toList)

extractSolutions :: Either String (Matrix Double) -> [Double]
extractSolutions (Left _) = []
extractSolutions (Right mat) = toList $ getCol (ncols mat) mat

makeLinear :: (Double, Double) -> Double -> Double
makeLinear (b, a) x = a*x+b

makeQuadratic :: (Double, Double, Double) -> Double -> Double
makeQuadratic (c, b, a) x = a*x**2+b*x+c

makeCubic :: (Double, Double, Double, Double) -> Double -> Double
makeCubic (d, c, b, a) x = a*x**3+b*x**2+c*x+d

makePower :: (Double, Double) -> Double -> Double
makePower (a, b) x = a*x**b

makeExponential :: (Double, Double) -> Double -> Double
makeExponential (a, b) x = a*(exp $ b*x)

makeLogarithmic :: (Double, Double) -> Double -> Double
makeLogarithmic (a, b) x = a+b*(log x)

solveLinear :: [(Double, Double)] -> Maybe (Double, Double)
solveLinear points = result where
    result = if length points < 2 || length ss < 2
        then Nothing
        else Just (ss!!0, ss!!1)
    ss = extractSolutions $ rref mat
    mat = fromLists
        [ [n,  sx,  sy]
        , [sx, sxx, sxy]
        ]
    n = fromIntegral $ length xs
    sx = sum xs
    sy = sum ys
    sxx = sum $ map (**2) xs
    sxy = sum $ zipWith (*) xs ys
    (xs, ys) = unzip points

solveQuadratic
    :: [(Double, Double)]
    -> Maybe (Double, Double, Double)
solveQuadratic points = result where
    result = if length points < 3 || length ss < 3
        then Nothing
        else Just (ss!!0, ss!!1, ss!!2)
    ss = extractSolutions $ rref mat
    mat = fromLists
        [ [n,   sx,  sxx, sy]
        , [sx,  sxx, sx3, sxy]
        , [sxx, sx3, sx4, sxxy]
        ]
    n = fromIntegral $ length xs
    sx = sum xs
    sy = sum ys
    sxx = sum $ map (**2) xs
    sxy = sum $ zipWith (*) xs ys
    sx3 = sum $ map (**3) xs
    sxxy = sum $ zipWith (*) ys $ map (**2) xs
    sx4 = sum $ map (**4) xs
    (xs, ys) = unzip points

solveCubic
    :: [(Double, Double)]
    -> Maybe (Double, Double, Double, Double)
solveCubic points = result where
    result = if length points < 4 || length ss < 4
        then Nothing
        else Just (ss!!0, ss!!1, ss!!2, ss!!3)
    ss = extractSolutions $ rref mat
    mat = fromLists
        [ [n,   sx,  sxx, sx3, sy]
        , [sx,  sxx, sx3, sx4, sxy]
        , [sxx, sx3, sx4, sx5, sxxy]
        , [sx3, sx4, sx5, sx6, sx3y]
        ]
    n = fromIntegral $ length xs
    sx = sum xs
    sy = sum ys
    sxx = sum $ map (**2) xs
    sxy = sum $ zipWith (*) xs ys
    sx3 = sum $ map (**3) xs
    sxxy = sum $ zipWith (*) ys $ map (**2) xs
    sx4 = sum $ map (**4) xs
    sx3y = sum $ zipWith (*) ys $ map (**3) xs
    sx5 = sum $ map (**5) xs
    sx6 = sum $ map (**6) xs
    (xs, ys) = unzip points

solvePower :: [(Double, Double)] -> Maybe (Double, Double)
solvePower points = f <$> result where
    f (a, b) = (exp a, b)
    result = solveLinear (zip (map log xs') (map log ys'))
    (xs', ys') = unzip $ filter (\(x, y) -> x > 0 && y > 0) points

solveExponential :: [(Double, Double)] -> Maybe (Double, Double)
solveExponential points = f <$> result where
    f (a, b) = (exp a, b)
    result = solveLinear (zip xs' (map log ys'))
    (xs', ys') = unzip $ filter (\(_, y) -> y > 0) points

solveLogarithmic :: [(Double, Double)] -> Maybe (Double, Double)
solveLogarithmic points = solveLinear (zip (map log xs') ys') where
    (xs', ys') = unzip $ filter (\(x, _) -> x > 0) points

getDeviation :: [(Double, Double)] -> (Double -> Double) -> Double
getDeviation points f = sqrt $ (sum ds)/(fromIntegral n) where
    ds = (\(x, y) -> ((f x) - y)**2) <$> points
    n = length points

getPearson :: [(Double, Double)] -> Double
getPearson points = u/(sqrt (sxs*sys)) where
    xm = (sum xs)/(fromIntegral $ length xs)
    ym = (sum ys)/(fromIntegral $ length ys)
    u = sum $ zipWith (\x y -> (x-xm)*(y-ym)) xs ys
    sxs = sum $ map (\x -> (x-xm)**2) xs
    sys = sum $ map (\y -> (y-ym)**2) ys
    (xs, ys) = unzip points
