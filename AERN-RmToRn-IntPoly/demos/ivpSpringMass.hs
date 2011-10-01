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

import qualified Data.Map as Map
import qualified Data.List as List

type Poly = IntPoly String MI.MI

shouldPrintIterations = False
--shouldPrintIterations = True
--shouldPrintPrecision = False
shouldPrintPrecision = True

field y = (-100 :: Int) |<*> y

main =
    do
    hSetBuffering stdout NoBuffering
    print initParams 
    putStrLn $ "initial state: " ++ show (fst $ head steps)
    mapM_ printStep $ tail steps 
    print initParams 
--    mapM_ print $ paramSearch 0 [initParams]
    where
    steps = simulate initParams

data Params =
    Params
    {
        paramStepSize :: Int, -- 2^{-s} step size
        paramStepEpsilon :: Int, -- 2^{-n} stop iterating Picard when the width change falls below this
        paramPrecision :: Precision, -- floating point precision
        paramDegree :: Int, -- maximal polynomial degree
--        paramTermSize :: Int, -- maximal polynomial term size
        paramRangeTolerance :: Int 
            -- 2^{-r} how hard to try to estimate ranges of polynomials 
            --        by splitting - currently unused
    }
    deriving (Show)

---- the following are taken from SpringMassV.hs, the timings in comments may not be right any more:
--initParams = Params 1 50 50 50 10 -- gets to time 6.5 in 0.27s (1 in 0.04s, 3.6 in 0.15s)
initParams = Params 0 100 100 80 10 -- gets to time 99 in 7s (36 in 2.3s)
--initParams = Params 0 200 200 160 10 -- gets to time 312 in 49s 
--initParams = Params (-2) 200 200 190 10 -- (?) gets to time 808 in 111s (360 in 49.5s)
--initParams = Params (-3) 300 300 350 10 -- (?) gets to time 3176 in 37.25min
--initParams = Params (-2) 300 300 300 10 -- (?) gets to time 1400 in 15min
--initParams = Params (-3) 400 400 450 10 -- (?) gets to time 8744 in 2.5h (3600 in 1h2min) starts at e-82 drops at e-35 using 2.2GB RAM
--initParams = Params (-3) 600 600 800 10 -- (?) gets to time 19768 in 11h55min starts at e-142 drops at e-35 using 3.5GB RAM
--initParams = Params (-4) 600 600 800 10 -- (?) gets to time 7712 in 6.5h (3600 in 3h) drops at e-70
--initParams = Params (-3) 700 700 850 10 -- (?) gets to time 25448 in 20h using 4.25GB RAM
--initParams = Params (-3) 900 900 950 10 -- (?) gets to time 41352 in 16h20min using 5GB RAM (36000 in 14h15min)

increaseParams (Params stp eps p dg rtol) =
    [
     Params stp eps p dgNew rtol, 
     Params stp epsNew pNew dg rtol, 
     Params stp epsNew pNew dgNew rtol
    ]
    where
    pNew = p + 2000
    dgNew = dg + 200
    epsNew = eps + 2000

paramSearch prevBest nextParamsOptions@(params : rest)
    | prevBest > 36000 = []
    | otherwise =
        (params, currentScoreSeq, currentScore) : 
            (case (rest, currentScore > prevBest + 1) of
                ([], _) -> paramSearch currentScore $ increaseParams params
                (_, True) -> paramSearch currentScore $ increaseParams params
                _ -> paramSearch prevBest rest
            )
         where
         currentScoreSeq =
            drop100ifAny 0 $ zip [0..] steps
         drop100ifAny n [] = []
         drop100ifAny n list 
            = 
            (n `div` stepsInOne) 
            : (drop100ifAny (n + 100) $ drop 100 list)
         currentScore =
            (length steps) `div` stepsInOne
         steps = simulate params
         stepsInOne = 2 ^ (paramStepSize params)

simulate params =
    waitTillPrecBelow (2^^(-30)) $ -- stop when diverging
        iterate (makeStep params stepSize epsilon) 
            ((tInit, (y0Init, y0Init), (y0DerInit, y0DerInit)),[]) -- initial values
    where
    tInit = i2mi 0
    y0Init = i2mi 20
    y0DerInit = i2mi 0
    waitTillPrecBelow eps (this@((_,yNmOnePair,_),_):rest)
        | precGood = this : (waitTillPrecBelow eps rest)
        | otherwise = [this]
        where
        yNmOne = joinPair yNmOnePair
        precGood = 
            case MI.width yNmOne MI.<? eps of
                Just True -> True
                _ -> False
    epsilon =
        (i2mi 1) MI.</> (i2mi $ 2^(paramStepEpsilon params))
    stepSize
        | stp >= 0 =(i2mi 1) MI.</> (i2mi $ 2^stp)
        | otherwise = i2mi $ 2^(- stp)
        where
        stp = paramStepSize params
    i2mi :: Integer -> MI.MI
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
    printIter (n, (((yPair,yDerPair),(yPoly,yDerPoly)), ((yPrevPair,yDerPrevPair),_))) =
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
    
makeStep params h epsilon ((t, y0Pair, y0DerPair), prevStepIters) =
--    unsafePrint
--    (
--        "makeStep:"
--        ++ "\n h = " ++ showMI h
--        ++ "\n t = " ++ showMI t
--        ++ "\n y0 = " ++ showMI y0
--        ++ "\n y0Der = " ++ showMI y0Der
--        ++ "\n yh = " ++ showMI yh
--        ++ "\n yhDer = " ++ showMI yhDer
--    )
    ((t + h, yhPair, yhDerPair), newStepIters)
    where
--    showMI = showInternals (20, shouldPrintPrecision)
    ((yhPair, yhDerPair), newStepIters) 
        = waitTillNoImprovementCheckInclusion False 1000 [] intPolySequence
    intPolySequence = map evalBoth polySequence
    polySequence
        =
        iterate (picard params c0 (y0Poly, y0DerPoly)) $ 
        (yFirstEncl, yDerFirstEncl)
        
    yFirstEncl
        = newConstFn cfg dombox $ y0 MI.<+> hToMinusH
    yDerFirstEncl
        = newConstFn cfg dombox $ y0Der MI.<+> hToMinusH
    y0 = joinPair y0Pair
    y0Der = joinPair y0DerPair
    y0Poly = newProjection cfg dombox "y0"
    y0DerPoly = newProjection cfg dombox "y0Der"
    
    cfg = 
        IntPolyCfg 
            {
                ipolycfg_vars = vars,
                ipolycfg_doms = doms,
                ipolycfg_sample_cf = h,
                ipolycfg_maxdeg = paramDegree params,
                ipolycfg_maxsize = 0 -- not used at the moment
            }
    dombox = Map.fromList $ zip vars doms
    vars = ["u","y0","y0Der"]
    doms = [(0,h), y0Pair, y0DerPair]

    evalBoth (p1,p2) = ((evalOne p1, evalOne p2), (p1, p2))
        where
        evalOne = refinePair .  evalPolyMono prec c0 [(h,h),y0Pair,y0DerPair]

    hToMinusH = (-h) MI.</\> h

    rtol = 
        (i2mi 1) MI.</> (i2mi $ 2^(paramRangeTolerance params))
    i2mi :: Integer -> MI.MI
    i2mi n = ArithInOut.convertOutEff prec n
    prec = paramPrecision params
    c0 = i2mi 0
    c1 = i2mi 1 
    waitTillNoImprovementCheckInclusion 
            wasInclusion maxIters prevIters 
            (this@(thisInts@(yNmOnePair,_),_):rest@(((yNPair,_),_):_)) 
        | maxIters < 0 = (thisInts, reverse currIters)
--        | wasInclusion && (not isInclusion) =
--            error "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX internal error: inclusion ceased"
        | (not isInclusion) || improvementAboveEpsilon  = 
            waitTillNoImprovementCheckInclusion isInclusion (maxIters - 1) currIters rest
        | otherwise = (thisInts, reverse currIters)
        where
        yNmOne = joinPair yNmOnePair
        yN = joinPair yNPair
        currIters = this : prevIters
        isInclusion =
            case yNmOne MI.|<=? yN of
                Just r -> r
                _ -> False 
        improvementAboveEpsilon =
            case yNmOneWidth - yNWidth MI.>? epsilon of
                Just True -> True
                _ -> False 
        yNmOneWidth = MI.width yNmOne
        yNWidth = MI.width yN         
    


picard ::
    Params ->
--    MI {-^ starting time -} -> -- autonomous system, always 0 
    MI.MI {-^ zero with correct precision -} ->
    (Poly, Poly) {-^ initial values for y,y' -} -> 
    (Poly, Poly) {-^ approximates of y and y' -} -> 
    (Poly, Poly) {-^ improved approximates of y and y' -}
picard params z (y0, y0Der) (yPrev, yPrevDer) =
    (yNext, yNextDer)
    where
    yNext =
        integratePolyMainVar prec z y0 yNextDer
    yNextDer =
        integratePolyMainVar prec z y0Der (field yPrev)
    prec = paramPrecision params


joinPair (l,r) = l MI.</\> r

refinePair (l,r) = (MI.Interval lL lL, MI.Interval rR rR)
    where
    MI.Interval lL _ = l
    MI.Interval _ rR = r
