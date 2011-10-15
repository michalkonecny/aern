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

resultTolerance :: MI.MI
--resultTolerance = (2^^(-30))
resultTolerance = (10^^(-4))

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
        paramStepSize :: MI.MI, -- step size
        paramStepEpsilon :: Int, -- 2^{-n} stop iterating Picard when the width change falls below this
        paramPrecision :: Precision, -- floating point precision
        paramDegree :: Int, -- maximal polynomial degree
--        paramTermSize :: Int, -- maximal polynomial term size
        paramRangeTolerance :: Int 
            -- 2^{-r} how hard to try to estimate ranges of polynomials 
            --        by splitting - currently unused
    }
    deriving (Show)

-- timings and stretch using tolerance 2^(-30):
--initParams = Params 0.5 50 50 50 10 -- gets to time 6.5 in 0.27s (1 in 0.04s, 3.6 in 0.15s)
--initParams = Params 1 100 100 80 10 -- gets to time 99 in 7s (36 in 2.3s)
--initParams = Params 1 200 200 160 10 -- gets to time 312 in 49s 
--initParams = Params 4 200 200 190 10 -- gets to time 808 in 111s (360 in 49.5s)
--initParams = Params 8 300 300 350 10 -- (?) gets to time 3176
--initParams = Params 8 400 400 450 10 -- (?) gets to time 8744
--initParams = Params 8 600 600 800 10 -- (?) gets to time 19768
--initParams = Params 16 600 600 800 10 -- (?) gets to time 7712
--initParams = Params 8 700 700 850 10 -- (?) gets to time 25448
--initParams = Params 8 900 900 950 10 -- (?) gets to time 41352

-- timings and stretch using tolerance 10^(-4):
--initParams = Params 2 80 80 0 10 -- gets to time 136 in (dev: 6.8s)
--initParams = Params 2 100 100 0 10 -- gets to time 234 in (dev: 14.7s)
--initParams = Params 2 120 120 0 10 -- gets to time 334 in (dev: 25s)
--initParams = Params 2 160 160 0 10 -- gets to time 532 in (dev: 52s)

--initParams = Params 4 150 150 0 10 -- gets to time 152 in (dev: 14s)
--initParams = Params 4 200 200 0 10 -- gets to time 944 in (dev: 116s)
--initParams = Params 4 250 250 0 10 -- gets to time 1344 in (dev: 214s = 3min 34s)
--initParams = Params 4 300 300 0 10 -- gets to time 1748 in (dev: 360s = 6min)

--initParams = Params 8 150 150 0 10 -- gets to time 360 in (dev: 38s)
--initParams = Params 8 180 180 0 10 -- gets to time 2032 in (dev: 257s = 4min 17s) -- improvement by incrfe 
--initParams = Params 8 200 200 0 10 -- gets to time 3152 in (dev: 498s = 8min 18s)
--initParams = Params 8 210 210 0 10 -- gets to time 3712 in (mik: 437s = 7min17s; 3600 in 423s = 7min 3s at e-5)
--initParams = Params 8 220 220 0 10 -- gets to time 4272 in (mik: 538s = 8min58s; 3600 in 453s = 7min 33s at e-8)
--initParams = Params 8 250 250 0 10 -- gets to time 5944 in (mik: 813s = 13min33s; 3600 in 492s = 8min 12s at e-17)
--initParams = Params 8 270 270 0 10 -- gets to time 7064 in (mik: 1094s = 18min13s; 3600 in 558s = 9min 18s at e-23)
--initParams = Params 8 790 790 0 10 -- gets to time 36136 in (mik: 22157s = 6h 9min; t=36000 in 22073s = 6h 8min at e-5)
initParams = Params 8 800 800 0 10 -- gets to time 36696 in (mik: 30201s = 8h 23min; t=36000 in 29628s = 8h 13min at e-8)
--initParams = Params 8 850 850 0 10 -- gets to time 39488 in (mik: 34619s = 9h 37min; t=36000 in 31561s = 8h 46min at e-23)

--initParams = Params 10 200 200 0 10 -- gets to time 650 in (dev: 100s)
--initParams = Params 10 220 220 0 10 -- gets to time 1090 in (dev: 200s)
--initParams = Params 10 250 250 0 10 -- gets to time 1750 in (dev: 362s)
--initParams = Params 10 300 300 0 10 -- gets to time 2850 in (dev: 791s)
--initParams = Params 10 600 600 0 10 -- gets to time 9480 in (mik: 82min)
--initParams = Params 10 700 700 0 10 -- gets to time 11690 in (mik: 126min)
--initParams = Params 10 800 800 0 10 -- gets to time ? in (mik: ?min)
--initParams = Params 10 1500 1500 0 10 -- gets to time ? in (mik: ?min)

--initParams = Params 12 600 600 0 10 -- gets to time 10008 in ? (dev: 154min 13s)
--initParams = Params 12 700 700 0 10 -- gets to time 12504 in ? (dev: 204min 9s)
--initParams = Params 12 800 800 0 10 -- gets to time 15000 in ? (dev: 338min 10s)

--initParams = Params 16 280 280 0 10 -- gets to time 672
--initParams = Params 16 600 600 0 10 -- gets to time (approx 20000)
--initParams = Params 16 700 700 0 10 -- gets to time (approx 25000)
--initParams = Params 16 800 800 0 10 -- gets to time (approx 32000)
--initParams = Params 16 850 850 0 10 -- gets to time 36784 in (mik:  51696s = 14.36h)  

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

--paramSearch prevBest nextParamsOptions@(params : rest)
--    | prevBest > 36000 = []
--    | otherwise =
--        (params, currentScoreSeq, currentScore) : 
--            (case (rest, currentScore > prevBest + 1) of
--                ([], _) -> paramSearch currentScore $ increaseParams params
--                (_, True) -> paramSearch currentScore $ increaseParams params
--                _ -> paramSearch prevBest rest
--            )
--         where
--         currentScoreSeq =
--            drop100ifAny 0 $ zip [0..] steps
--         drop100ifAny n [] = []
--         drop100ifAny n list 
--            = 
--            (n `div` stepsInOne) 
--            : (drop100ifAny (n + 100) $ drop 100 list)
--         currentScore =
--            (length steps) `div` stepsInOne
--         steps = simulate params
--         stepsInOne = 2 ^ (paramStepSize params)

simulate params =
    waitTillPrecBelow resultTolerance $ -- stop when diverging
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
    stepSize = (i2mi 1) MI.<*> paramStepSize params
--        | stp >= 0 =(i2mi 1) MI.</> (i2mi $ 2^stp)
--        | otherwise = i2mi $ 2^(- stp)
--        where
--        stp = paramStepSize params
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
        evalOne = refinePair .  evalPolyOnInterval prec c0 [(h,h),y0Pair,y0DerPair]

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
