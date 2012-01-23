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

{-
    approximations to exact solution computed so far:

    angle(0.5) = -0.0239512848529275...
    angle(1)   = -0.998949814623850...
    angle(1.5) =  0.0717940933102...
    angle(2)   =  0.995800677124...
    angle(2.5) = -0.1194579036...
    angle(3)   = -0.990556855...
    angle(3.5) =  0.166824...
    angle(4)   =  0.9832...

-}

resultTolerance :: MI.MI
--resultTolerance = (2^^(-30))
resultTolerance = (10^^(-4))

maxConsecutiveFailuresToImproveEnclosure :: Int
maxConsecutiveFailuresToImproveEnclosure = 2

maxInclusionFailures :: Int
maxInclusionFailures = 20

{- experiments using tolerance 2^(-4):
    
    -- experiments with different step sizes:
    
    bin/ivpPendulum simulate 3 40 100 10 100 5  -- gets to time 0.25 in (mik: 2s)
    
    bin/ivpPendulum simulate 4 40 100 10 100 5  -- gets to time 1.1875 in (mik: 14s)
    
    bin/ivpPendulum simulate 5 40 100 10 100 5  -- gets to time 1.9375 in (mik: 37s)
    
    bin/ivpPendulum simulate 6 40 100 10 100 5  -- gets to time 1.8125 in (mik: 58s)
    

    bin/ivpPendulum simulate 3 40 100 16 140 6  -- gets to time 0.375 in (mik: 20s)
    
    bin/ivpPendulum simulate 4 40 100 16 140 6 -- get to time 1.375 in (mik: 100s)

    bin/ivpPendulum simulate 5 40 100 16 140 6 -- get to time 1.9375 in (mik: 229s = 3min49s)

    bin/ivpPendulum simulate 6 40 100 16 140 6 -- get to time 2.203125 in (mik: 7min)
    

    bin/ivpPendulum simulate 4 40 100 18 300 8  -- gets to time 3.25 in (mik: 13min24s)
   
    bin/ivpPendulum simulate 5 40 100 18 300 8  -- gets to time 4.28125 in (mik: 29min10s)
   
    bin/ivpPendulum simulate 6 40 100 18 300 8  -- gets to time 3.90625 in (mik: 50min47s)


    bin/ivpPendulum simulate 3 40 400 60 600 7 -- get to time 1 in (mik: 22min)

    bin/ivpPendulum simulate 4 40 400 60 600 7 -- get to time 3.125 in (mik: 83min)

    -- step size 5:
    
    bin/ivpPendulum simulate 5 40 100 10 100 5  -- gets to time 1.9375 in (mik: 34s)
    
      bin/ivpPendulum simulate 5 40 100 10 100 6  -- gets to time 2.0625 in (mik: 41s)
    
      bin/ivpPendulum simulate 5 40 100 12 100 5  -- gets to time 1.96875 in (mik: 76s)
    
      bin/ivpPendulum simulate 5 40 100 10 120 5  -- gets to time 1.9375 in (mik: 35s)
    
      bin/ivpPendulum simulate 5 50 100 10 100 5  -- gets to time 1.9375 in (mik: 39s)
    
    bin/ivpPendulum simulate 5 40 100 10 100 6  -- gets to time 2.0625 in (mik: 41s)
    
      bin/ivpPendulum simulate 5 40 100 10 100 7  -- gets to time 2.0625 in (mik: 46s)
    
      bin/ivpPendulum simulate 5 40 100 12 100 6  -- gets to time 2.625 in (mik: 120s)
    
      bin/ivpPendulum simulate 5 40 100 10 120 6  -- gets to time 2.0625 in (mik: 41s)
    
    bin/ivpPendulum simulate 5 40 100 12 100 6  -- gets to time 2.625 in (mik: 120s)

    bin/ivpPendulum simulate 5 50 100 14 160 7  -- gets to time 3.28125 in (mik: 305s = 6min5s)
   
    bin/ivpPendulum simulate 5 40 100 18 300 8  -- gets to time 4.28125 in (mik: 29min10s)
   
   
   
    -- step size 3 (turned out to be a bad choice...):
    
    bin/ivpPendulum simulate 3 50 100 20 240 7  -- gets to time 0.75 in (dev: 3min 2s)
    
    bin/ivpPendulum simulate 3 40 400 60 600 10 -- get to time 1 in (mik: 48min)
    
    bin/ivpPendulum simulate 3 100 1000 100 1000 7 -- get to time 1.75 in (mik: 3h28min)

    bin/ivpPendulum simulate 3 50 600 100 1000 10 -- get to time 1.75 in (dev: 7h8min)

    bin/ivpPendulum simulate 3 100 1000 100 1000 13 -- get to time 1.75 in (mik: 5h7min)


    bin/ivpPendulum simulate 4 40 400 100 1000 8 -- get to time ? in (mik: ?min)

-}

shouldPrintIterations = False
--shouldPrintIterations = True
shouldPrintPrecision = False
--shouldPrintPrecision = True

main =
    do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    let (mode : paramsS) = args
    let initParams = parseParams paramsS
    print initParams
    case mode of
        "simulate" ->
            do
            let steps = simulate initParams
            putStrLn $ "initial state: " ++ show (fst $ head steps)
            mapM_ printStep $ tail steps
            print initParams 
        "search" ->
            do
            mapM_ printParamSearchStep $ paramSearch 0 initParams

data Params =
    Params
    {
        paramStepSize :: Int, -- 2^{-n} step size
        paramStepEpsilon :: Int, -- 2^{-n} stop iterating Picard when the width change falls below this
        paramPrecision :: Precision, -- floating point precision
        paramDegree :: Int, -- maximal polynomial degree
        paramTermSize :: Int, -- maximal polynomial term size
        paramSineTerms :: Int -- how many terms of the Taylor expansion should be computed
    }
--    deriving (Show)
    
instance Show Params where
    show (Params stp eps p dg sz sinet) =
        "Params{" ++ (List.intercalate " " $ map show [stp,eps,fromIntegral p,dg,sz,sinet]) ++ "}" 

parseParams [stepSizeS, stepEpsS, precS, maxDegS, maxSizeS, sineTermsS]
    =
    Params
    {
        paramStepSize = read stepSizeS,
        paramStepEpsilon = read stepEpsS,
        paramPrecision = fromInteger $ read precS,
        paramDegree = read maxDegS,
        paramTermSize = read maxSizeS,
        paramSineTerms = read sineTermsS
    }
   
    
increaseIndividualParams (Params stp eps p dg sz sinet) =
    [
     Params stp eps p dgNew sz sinet, 
     Params stp eps p dg szNew sinet, 
     Params stp eps p dg sz sinetNew,
     Params stp epsNew p dg sz sinet,
     Params stp eps pNew dg sz sinet
    ]
    where
    epsNew = eps + 10 
    pNew = p + 50
    dgNew = dg + 2
    szNew = sz + 20
    sinetNew = sinet + 1

increaseAllParams (Params stp eps p dg sz sinet) =
    Params stp epsNew pNew dgNew szNew sinetNew
    where
    epsNew = eps + 10 
    pNew = p + 50
    dgNew = dg + 5
    szNew = sz + 20
    sinetNew = sinet + 1

paramSearch prevScore prevParams
    | nextScore > 360 * stepsInOne = [(scoresParams, nextScore, nextParams)]
    | otherwise = 
        (scoresParams, nextScore, nextParams) : (paramSearch nextScore nextParams)
    where
    (nextScore, nextParams) 
        = (bestScore, bestParams)
--        | otherwise = (getScore $ simulate incAllParams, incAllParams)
    incAllParams = increaseAllParams prevParams
    (bestScore, bestParams) = 
        head $ List.sortBy (\a b -> compare (fst b) (fst a)) scoresParams
    scoresParams = zip scores paramsList
    scores = map getScore results
    results = map simulate paramsList
    paramsList = increaseIndividualParams prevParams 
    getScore steps = (length steps) -- `div` stepsInOne
    stepsInOne = 2 ^ (paramStepSize prevParams)
    
printParamSearchStep (scoresParams, chosenScore, chosenParams) 
    =
    do
    putStrLn "*** Trying various ways to get the simulation further:"
    mapM_ printAttempt scoresParams
    putStrLn $ "proceeding with score " ++ show chosenScore ++ " with choosen params = " ++ show chosenParams
    where
    printAttempt (score, params) =
        do
        putStrLn $ "got score " ++ show score ++ " using params = " ++ show params
    
simulate params =
    waitTillPrecBelow resultTolerance $ -- stop when diverging
        iterate (makeStep params stepSize epsilon)
            (Just (tInit, y0Init, yDer0Init), []) -- initial values
    where
    tInit = i2mi 0
    y0Init = i2mi initAngle
    yDer0Init = i2mi initAngularSpeed
    waitTillPrecBelow eps ((Nothing,_):rest) = []
    waitTillPrecBelow eps ((Just this@(_,yNmOne,_),iterInfo):rest)
        | precGood = (this, iterInfo) : (waitTillPrecBelow eps rest)
        | otherwise = [(this, iterInfo)]
        where
        precGood = 
            case MI.width yNmOne MI.<? eps of
                Just True -> True
                _ -> False
    epsilon =
        (i2mi 1) MI.</> (i2mi $ 2^(paramStepEpsilon params))
    stepSize 
--        = (i2mi 1) MI.<*> paramStepSize params
        | stp >= 0 =(i2mi 1) MI.</> (i2mi $ 2^stp)
        | otherwise = i2mi $ 2^(- stp)
        where
        stp = paramStepSize params
    i2mi :: Rational -> MI.MI
    i2mi n =
        ArithInOut.convertOutEff (paramPrecision params) n
    
printStep ((t,y,yDer), iters) =
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
    printIter (n, ((_,(y,yDer),(yPoly,yDerPoly)), (_,(yPrev,yDerPrev),_))) =
        do
        putStrLn $ " >>>>>>>>>>>>>>>>> iteration " ++ show n ++ ":"
        putStrLn $ "  y = " ++ showP yPoly ++ "; "
        putStrLn $ "  y' = " ++ showP yDerPoly ++ "; "
        putStrLn $ "  y(h) = " ++ show y ++ "; " ++ "y'(h) = " ++ show yDer ++ "; "
        putStrLn $ "  width of y(h) = " ++ showMI (MI.width y)
        putStrLn $ "  Improvement: " ++ show (yPrev MI.|<=? y)
    showMI = showInternals (30,shouldPrintPrecision)
    showP = showPoly id show
    
makeStep params h epsilon (Nothing, prevStepIters) = (Nothing, prevStepIters)
makeStep params h epsilon (Just (t, y0, yDer0), prevStepIters) =
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
        (Just (yh, yDerh), newStepIters) -> 
            (Just (t + h, yh, yDerh), newStepIters)
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
    doms = [(0 MI.</\> h), y0, yDer0]

    evalBoth (n,(p1,p2)) = (n, (evalOne p1, evalOne p2), (p1, p2))
        where
        evalOne = evalPolyOnInterval (effMI prec) [h,y0,yDer0]

    hToMinusH = (-h) MI.</\> h

    i2mi :: Integer -> MI.MI
    i2mi n = ArithInOut.convertOutEff prec n
    prec = paramPrecision params
    c0 = i2mi 0
    c1 = i2mi 1 
    waitTillInclusionThenNoImprovement
            maxNonInclusionIters 
            prevYs prevYWidths prevItersInfo
            (this@(n,thisInts@(yN,_),_):rest) 
        | maxNonInclusionIters < 0 = (Nothing, reverse currItersInfo)
        | isInclusion = 
--            unsafePrint
--                ("picard " ++ show n ++ " INCLUSION: width = " ++ show yNWidth
--                    ++ "\n encl = " ++ show yN
--                ) $
            waitTillNoImprovement 1000 currItersInfo thisInts yNWidth 0 rest
        | otherwise =
--            unsafePrint
--                ("picard " ++ show n ++ " NO INCLUSION: width = " ++ show yNWidth
--                    ++ "\n encl = " ++ show yN
--                ) $
            waitTillInclusionThenNoImprovement
                (maxNonInclusionIters - 1) 
                (yN : prevYs) (yNWidth : prevYWidths) currItersInfo
                rest
        where
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
            maxIters prevItersInfo bestPrevInts@(bestY,bestYDer) bestYWidth prevConsecutiveFailures
            (this@(n,thisInts@(yN,yDerN),_):rest) 
        | maxIters < 0 = 
--            unsafePrint("picard " ++ show n ++ " TOO MANY ITERATIONS: width = " ++ show yNIWidth) $
            (Just bestPrevInts, reverse currItersInfo)
        | continue = 
--            unsafePrint
--                ("picard " ++ show n ++ ": width = " ++ show yNIWidth 
--                    ++ "; raw width = " ++ show yNWidth
--                    ++ "\n raw encl = " ++ show yN
--                ) $
            waitTillNoImprovement 
                (maxIters - 1) currItersInfo newBestInts newBestWidth newConsecutiveFailures 
                rest
        | otherwise = 
--            unsafePrint("picard " ++ show n ++ " GIVING UP, BEST SO FAR: width = " ++ show bestYWidth) $
            (Just bestPrevInts, reverse currItersInfo)
        where
        yNWidth = MI.width yN
        yNI = yN MI.<\/> bestY
        yDerNI = yDerN MI.<\/> bestYDer
        yNIWidth = MI.width yNI
        currItersInfo = this : prevItersInfo
        improvementAboveEpsilon =
            (bestYWidth - yNIWidth MI.>? epsilon) == Just True
        newBestInts = (yNI, yDerNI)
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
--    unsafePrint ("picard " ++ show n ++ ":\n  yNext = " ++ showPoly show show yNext ++ "\n  yDerNext = " ++ showPoly show show yDerNext) $
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
        integratePolyMainVar (effMI prec) z y0 yDerNext
    yDerNext =
        integratePolyMainVar (effMI prec) z yDer0 yDerDerNext
    yDerDerNext = field (effMI prec) sineTerms yPrev
    prec = paramPrecision params
    sineTerms = paramSineTerms params

effMI prec = (prec, (prec, ()))

