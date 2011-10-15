module Main where

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly
import Numeric.AERN.RmToRn.New

import Numeric.AERN.RealArithmetic.Basis.MPFR
import qualified Numeric.AERN.MPFRBasis.Interval as MI

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import Numeric.AERN.Basics.ShowInternals

import Numeric.AERN.Misc.Debug

import System.IO
import System.Environment

import qualified Data.Map as Map
import qualified Data.List as List

type Poly = IntPoly String MI.MI

field prec sineTerms angle = (-100 :: Int) |<*> (sinePoly prec sineTerms angle)

initAngle :: Rational
initAngle = 1

initAngularSpeed :: Rational
initAngularSpeed = 0

resultTolerance :: MI.MI
--resultTolerance = (2^^(-30))
resultTolerance = (10^^(-4))

maxConsecutiveFailuresToImproveEnclosure :: Int
maxConsecutiveFailuresToImproveEnclosure = 10

maxInclusionFailures :: Int
maxInclusionFailures = 20

{- experiments using tolerance 2^(-4):
    
    bin/ivpPendulum 0.125 50 100 16 140 6  -- gets to time 0.5 in (dev: 59s)
    
    bin/ivpPendulum 0.125 50 100 20 240 7  -- gets to time 1.25 in (dev: 13min 54s)
-}

shouldPrintIterations = False
--shouldPrintIterations = True
shouldPrintPrecision = False
--shouldPrintPrecision = True

main =
    do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let initParams = parseArgs args
    print initParams
    let steps = simulate initParams
    putStrLn $ "initial state: " ++ show (fst $ head steps)
    mapM_ printStep $ tail steps 
    print initParams 

data Params =
    Params
    {
        paramStepSize :: MI.MI, -- step size
        paramStepEpsilon :: Int, -- 2^{-n} stop iterating Picard when the width change falls below this
        paramPrecision :: Precision, -- floating point precision
        paramDegree :: Int, -- maximal polynomial degree
        paramTermSize :: Int, -- maximal polynomial term size
        paramSineTerms :: Int -- how many terms of the Taylor expansion should be computed
    }
    deriving (Show)

parseArgs [stepSizeS, stepEpsS, precS, maxDegS, maxSizeS, sineTermsS]
    =
    Params
    {
        paramStepSize = fromRational $ toRational $ (read stepSizeS :: Double),
        paramStepEpsilon = read stepEpsS,
        paramPrecision = fromInteger $ read precS,
        paramDegree = read maxDegS,
        paramTermSize = read maxSizeS,
        paramSineTerms = read sineTermsS
    }
    
simulate params =
    waitTillPrecBelow resultTolerance $ -- stop when diverging
        iterate (makeStep params stepSize epsilon)
            (Just (tInit, (y0Init, y0Init), (yDer0Init, yDer0Init)),[]) -- initial values
    where
    tInit = i2mi 0
    y0Init = i2mi initAngle
    yDer0Init = i2mi initAngularSpeed
    waitTillPrecBelow eps ((Nothing,_):rest) = []
    waitTillPrecBelow eps ((Just this@(_,yNmOnePair,_),iterInfo):rest)
        | precGood = (this, iterInfo) : (waitTillPrecBelow eps rest)
        | otherwise = [(this, iterInfo)]
        where
        yNmOne = joinPair yNmOnePair
        precGood = 
            case MI.width yNmOne MI.<? eps of
                Just True -> True
                _ -> False
    epsilon =
        (i2mi 1) MI.</> (i2mi $ 2^(paramStepEpsilon params))
    stepSize = (i2mi 1) MI.<*> paramStepSize params
--        | stp >= 0 =(i2mi 1) MI.</> (i2mi $ 2^stp)
--        | otherwise = i2mi $ 2^(- stp)
--        where
--        stp = paramStepSize params
    i2mi :: Rational -> MI.MI
    i2mi n =
        ArithInOut.convertOutEff (paramPrecision params) n
    
printStep ((t,yPair,yDerPair), iters) =
    do
    putStrLn $ replicate 120 '*'
    putStrLn $ "iterating interval Picard for t = " ++ show t ++ ":"
    case shouldPrintIterations of
        True -> 
            mapM_ printIter $ zip [1..] $ zip (tail iters) iters
        False -> return ()
    putStrLn $ "after " ++ show (length iters) ++ " iterations stabilised on:" 
    putStrLn $ " t = " ++ show t ++ "; y(t) = " ++ showMI y ++ "; y'(t) = " ++ showMI yDer
    putStrLn $ " width of y = " ++ showMI (MI.width y)
    where
    y = joinPair yPair
    yDer = joinPair yDerPair
    printIter (n, ((_,(yPair,yDerPair),(yPoly,yDerPoly)), (_,(yPrevPair,yDerPrevPair),_))) =
        do
        putStrLn $ " >>>>>>>>>>>>>>>>> iteration " ++ show n ++ ":"
        putStrLn $ "  y = " ++ showP yPoly ++ "; "
        putStrLn $ "  y' = " ++ showP yDerPoly ++ "; "
        putStrLn $ "  y(h) = " ++ show y ++ "; " ++ "y'(h) = " ++ show yDer ++ "; "
        putStrLn $ "  width of y(h) = " ++ showMI (MI.width y)
        putStrLn $ "  Improvement: " ++ show (yPrev MI.|<=? y)
        where
        yPrev = joinPair yPrevPair
    showMI = showInternals (30,shouldPrintPrecision)
    showP = showPoly id show
    
makeStep params h epsilon (Nothing, prevStepIters) = (Nothing, prevStepIters)
makeStep params h epsilon (Just (t, y0Pair, yDer0Pair), prevStepIters) =
--    unsafePrint
--    (
--        "makeStep:"
--        ++ "\n h = " ++ showMI h
--        ++ "\n t = " ++ showMI t
--        ++ "\n y0 = " ++ showMI y0
--        ++ "\n yDer0 = " ++ showMI yDer0
--        ++ "\n yh = " ++ showMI yh
--        ++ "\n yhDer = " ++ showMI yhDer
--    )
    case waitTillInclusionThenNoImprovement maxInclusionFailures [] [] [] intPolySequence of
        (Just (yhPair, yDerhPair), newStepIters) -> 
            (Just (t + h, yhPair, yDerhPair), newStepIters)
        (Nothing, newStepIters) -> (Nothing, newStepIters)
    where
--    showMI = showInternals (20, shouldPrintPrecision)
    intPolySequence = map evalBoth polySequence
    polySequence
        =
        iterate (picard params c0 (y0Poly, yDer0Poly)) $ 
        (0,(yFirstEncl, yDerFirstEncl))
        
    yFirstEncl
        = newConstFn cfg dombox $ y0 MI.<+> hToMinusH
    yDerFirstEncl
        = newConstFn cfg dombox $ yDer0 MI.<+> hToMinusH
    y0 = joinPair y0Pair
    yDer0 = joinPair yDer0Pair
    y0Poly = newProjection cfg dombox "y0"
    yDer0Poly = newProjection cfg dombox "yDer0"
    
    cfg = 
        IntPolyCfg 
            {
                ipolycfg_vars = vars,
                ipolycfg_doms = doms,
                ipolycfg_sample_cf = h,
                ipolycfg_maxdeg = paramDegree params,
                ipolycfg_maxsize = paramTermSize params
            }
    dombox = Map.fromList $ zip vars doms
    vars = ["u","y0","yDer0"]
    doms = [(0,h), y0Pair, yDer0Pair]

    evalBoth (n,(p1,p2)) = (n, (evalOne p1, evalOne p2), (p1, p2))
        where
        evalOne = refinePair .  evalPolyOnInterval prec c0 [(h,h),y0Pair,yDer0Pair]

    hToMinusH = (-h) MI.</\> h

    i2mi :: Integer -> MI.MI
    i2mi n = ArithInOut.convertOutEff prec n
    prec = paramPrecision params
    c0 = i2mi 0
    c1 = i2mi 1 
    waitTillInclusionThenNoImprovement
            maxNonInclusionIters 
            prevYs prevYWidths prevItersInfo
            (this@(n,thisInts@(yNPair,_),_):rest) 
        | maxNonInclusionIters < 0 = (Nothing, reverse currItersInfo)
        | isInclusion = 
            unsafePrint("picard " ++ show n ++ " INCLUSION: width = " ++ show yNWidth) $
            waitTillNoImprovement 1000 currItersInfo thisInts yNWidth 0 rest
--        | noWidthImprovement = 
--            (Nothing, reverse currItersInfo)
        | otherwise =
            unsafePrint("picard " ++ show n ++ ": width = " ++ show yNWidth) $
            waitTillInclusionThenNoImprovement
                (maxNonInclusionIters - 1) 
                (yN : prevYs) (yNWidth : prevYWidths) currItersInfo
                rest
        where
        yN = joinPair yNPair
        yNWidth = MI.width yN         
        currItersInfo = this : prevItersInfo
        isInclusion =
            or $ map yNincludedIn prevYs
            where
            yNincludedIn y =
                (y MI.|<=? yN) == Just True
--        noWidthImprovement =
--            (length prevYWidths >= 5) &&
--            (not $ or $ map yNSmallerThan $ take 5 prevYWidths)
--            where
--            yNSmallerThan yWidth = (yNWidth MI.<? yWidth) == Just True 
    waitTillNoImprovement 
            maxIters prevItersInfo bestPrevInts@(bestYPair,bestYDerPair) bestYWidth prevConsecutiveFailures
            (this@(n,thisInts@(yNPair,yDerNPair),_):rest) 
        | maxIters < 0 = 
            unsafePrint("picard " ++ show n ++ ": width = " ++ show yNIWidth) $
            (Just bestPrevInts, reverse currItersInfo)
        | continue = 
            unsafePrint("picard " ++ show n ++ ": width = " ++ show yNIWidth) $
            waitTillNoImprovement 
                (maxIters - 1) currItersInfo newBestInts newBestWidth newConsecutiveFailures 
                rest
        | otherwise = 
            unsafePrint("picard " ++ show n ++ " GIVING UP, BEST SO FAR: width = " ++ show bestYWidth) $
            (Just bestPrevInts, reverse currItersInfo)
        where
        yN = joinPair yNPair
        yDerN = joinPair yDerNPair
        yNI = yN MI.<\/> (joinPair bestYPair)
        yDerNI = yDerN MI.<\/> (joinPair bestYDerPair)
        yNIWidth = MI.width yNI
        currItersInfo = this : prevItersInfo
        improvementAboveEpsilon =
            (bestYWidth - yNIWidth MI.>? epsilon) == Just True
        newBestInts = (refinePair (yNI, yNI), refinePair (yDerNI, yDerNI))
        newBestWidth = yNIWidth
        (continue, newConsecutiveFailures)
            | improvementAboveEpsilon = (True, 0)
            | otherwise =
                (prevConsecutiveFailures < maxConsecutiveFailuresToImproveEnclosure, 
                 prevConsecutiveFailures + 1)


picard ::
    Params ->
--    MI {-^ starting time -} -> -- autonomous system, always 0 
    MI.MI {-^ zero with correct precision -} ->
    (Poly, Poly) {-^ initial values for y,y' -} -> 
    (Int, (Poly, Poly)) {-^ approximates of y and y' -} -> 
    (Int, (Poly, Poly)) {-^ improved approximates of y and y' -}
picard params z (y0, yDer0) (n,(yPrev, yDerPrev)) =
--    unsafePrint ("picard " ++ show n) $
--    unsafePrint
--    (
--        "picard:"
--        ++ "\n y0 = " ++ show y0
--        ++ "\n yDer0 = " ++ show yDer0
--        ++ "\n yPrev = " ++ show yPrev
--        ++ "\n yDerDerNext = " ++ show yDerDerNext
--        ++ "\n yDerNext = " ++ show yDerNext
--        ++ "\n yNext = " ++ show yNext
--    ) $
    (n+1,(yNext, yDerNext))
    where
    yNext =
        integratePolyMainVar prec z y0 yDerNext
    yDerNext =
        integratePolyMainVar prec z yDer0 yDerDerNext
    yDerDerNext = field prec sineTerms yPrev
    prec = paramPrecision params
    sineTerms = paramSineTerms params


joinPair (l,r) = l MI.</\> r

refinePair (l,r) = (MI.Interval lL lL, MI.Interval rR rR)
    where
    MI.Interval lL _ = l
    MI.Interval _ rR = r
