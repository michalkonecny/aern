module Main where

import Numeric.AERN.Poly.IntPoly (IntPoly(..), defaultIntPolySizeLimits, IntPolySizeLimits(..))
import Numeric.AERN.Poly.IntPoly.Plot (CairoDrawEffortIndicatorFnFromEval(..))
import Numeric.AERN.Basics.Interval (Interval)

import Numeric.AERN.RmToRn 
        (Domain, fromAscList, getVarDoms, 
         newConstFn, newProjection,
         evaluationDefaultEffort, evalAtPointOutEff)

import Numeric.AERN.RealArithmetic.Basis.Double ()
--import Numeric.AERN.RealArithmetic.Basis.MPFR

-- real arithmetic operators and imprecision measure:
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators ((|<*>), (>+<))
import Numeric.AERN.RealArithmetic.RefinementOrderRounding 
    (RoundedReal, roundedRealDefaultEffort,
     expOutEff, expInEff, expDefaultEffort)
import Numeric.AERN.RealArithmetic.Measures (imprecisionOf, iterateUntilAccurate, iterateUntilAccurate2)
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.ElementaryFromFieldOps.Exponentiation
        (ExpThinEffortIndicator(..))

import Numeric.AERN.RefinementOrder ((</\>), (>/\<))


import qualified 
       Numeric.AERN.RmToRn.Plot.FnView 
       as FV
import Numeric.AERN.RmToRn.Plot.CairoDrawable (cairoDrawFnDefaultEffort)

import qualified 
       Graphics.UI.Gtk 
       as Gtk (unsafeInitGUIForThreadedRTS, mainGUI)

import Control.Concurrent.STM (atomically, newTVar)

import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import System.Environment (getArgs)


--type CF = Interval MPFR
type CF = Interval Double
type Poly = IntPoly String CF
type PI = Interval Poly
type Fn = PI

usage :: String
usage = "Usage: Inclusion <maxdeg> <exp taylor degree>"

processArgs :: [String] -> (Int, Int)
processArgs [maxdegS, expTaylorS] =
    case (reads maxdegS, reads expTaylorS) of
        ((maxdeg, []):_,(expTaylor, []):_) -> (maxdeg, expTaylor)
        _ -> error $ usage
processArgs _ = error $ usage

main :: IO ()
main =
    do
    -- set console to print each line asap:
    hSetBuffering stdout LineBuffering
    -- process command line arguments:
    args <- getArgs
    let (maxdeg, expTaylor) = processArgs args 

    -- plot the enclosures for all iterations computed in the last attempt:
    plot $ inclusionFunctions maxdeg expTaylor

inclusionFunctions :: Int -> Int -> ([[Fn]], FV.FnMetaData Fn)
inclusionFunctions maxdeg expTaylorDeg = (fnGroups, fnmeta)
    where
    fnGroups = [[expSXOut, expXpmDeltaOut, expXpmDeltaIn]]

    expXpmDeltaOut = expOutEff effExp xpmDeltaOut
    expXpmDeltaIn = expInEff effExp xpmDeltaIn

    xpmDeltaOut = x + (delta </\> (- delta))
    xpmDeltaIn = x >+< (delta >/\< (- delta))
    delta = newConstFn limits [("x", dom)] 0.1
    
    expSXOut = expOutEff effExp $ s * x
    s =  newConstFn limits [("x", dom)] 0.91
    
--    xPlus1 = x + 1
    x = newProjection limits [("x", dom)] "x"
    dom = (0) </\> 1
--    dom = (-0.2) </\> 0.2

    effExp = 
        (expDefaultEffort x)
        {
            expeff_taylorDeg = expTaylorDeg            
        }

    fnmeta =
        FV.simpleFnMetaData
            sampleFn 
            (FV.Rectangle  3 (0) (0) 1) -- initial plotting region
            (Just (1,1,1,1))
            200 -- samplesPerUnit
            "x"
            [("inclusion", functionInfos)]
        where
        sampleFn = x
        functionInfos =
            zip3 
                ["exp(0.9*x)", "Out(exp(x+-0.1))", "In(exp(x+-0.1))"]
                [black, blue, blue] -- styles 
                (repeat True) -- show everything by default

    limits = 
        (defaultIntPolySizeLimits sampleCF cfLimits arity)
        {
            ipolylimits_maxdeg = maxdeg
        }
        where
        sampleCF = 0
        cfLimits = ()
        arity = 1
    

logisticMapIterateNTimes ::
    (Num real, RoundedReal real)
    =>
    Rational {-^ @r@ scaling constant -} -> 
    real {-^ @x0@ initial value  -} ->
    Int {-^ @n@ number of iterations -} ->
    [real] {-^ @logisticMap^n(x0)@ -} 

logisticMapIterateNTimes r x0 n = 
    take (n+1) $ (iterate (logisticMap r) x0)
    
logisticMap ::
    (Num real, RoundedReal real)
    =>
    Rational {-^ scaling constant r -} -> 
    real {-^ previous value x -} ->
    real {-^ rx(1-x) -} 

logisticMap r xPrev =
    r |<*> (xPrev * (1 - xPrev))
        
addPlotMetainfo ::
    [Fn] ->
    ([[Fn]], FV.FnMetaData Fn)
addPlotMetainfo fns =
    ([fns], fnMeta)
    where
    fnMeta =  
            FV.simpleFnMetaData
                sampleFn 
                (FV.Rectangle  1 (0) 0 1) -- initial plotting region
                Nothing
                200 -- samplesPerUnit
                "x"
                [("LM iterations", functionInfos)]
            where
            functionInfos =
                zip3 
                    ["x" ++ show i | i <- [1..iters]]
                    (repeat blue) -- styles 
                    (repeat True) -- show all iterations by default
            iters = length fns
    sampleFn : _ = fns

plot :: ([[Fn]], FV.FnMetaData Fn) -> IO ()
plot (fns, fnmeta) =
    do
    putStrLn $ "last fn  = " ++ show lastFn
    putStrLn $ "impresision of iterations = " ++ show (map imprecisionOf $ head fns)
    -- enable multithreaded GUI:
    _ <- Gtk.unsafeInitGUIForThreadedRTS
    fnDataTV <- atomically $ newTVar $ FV.FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    _ <- FV.new sampleFn effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
--    Concurrent.forkIO $ signalFn fnMetaTV
    Gtk.mainGUI
    where
    ((sampleFn :_) :_) = fns 
    lastFn = head $ reverse $ head fns 
    effDrawFn = 
        (cairoDrawFnDefaultEffort sampleFn)
--        {
--            draweff_evalF = effEval
--        }
    effEval = 
        (evaluationDefaultEffort sampleFn)
--        (ipolyeff2, othereff)
--        where
--        ipolyeff2 =
--            ipolyeff
--            {
--                ipolyeff_evalMaxSplitSize = 1000
--            }
--        (ipolyeff, othereff) =
--            (evaluationDefaultEffort sampleFn)
    effCF = roundedRealDefaultEffort (0:: CF)
    --effCF = (100, (100,())) -- MPFR

addPlotVar :: [[Fn]] -> [[(FV.GraphOrParamPlotFn Fn, String)]]
addPlotVar fns =
    map (map addV) fns
    where
    addV fn = (FV.GraphPlotFn [fn], plotVar)
        where
        (plotVar : _) = vars
        vars = map fst $ getVarDoms fn   

black :: FV.FnPlotStyle
black = FV.defaultFnPlotStyle
blue :: FV.FnPlotStyle
blue = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.1,0.1,0.8,1), 
        FV.styleFillColour = Just (0.1,0.1,0.8,0.1) 
    } 
red :: FV.FnPlotStyle
red = FV.defaultFnPlotStyle 
    { 
        FV.styleOutlineColour = Just (0.8,0.2,0.2,1), 
        FV.styleFillColour = Just (0.8,0.2,0.2,0.1) 
    } 

