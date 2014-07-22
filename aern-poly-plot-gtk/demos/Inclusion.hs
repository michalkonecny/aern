module Main where

---- Imports -----

import Numeric.AERN.Poly.IntPoly (IntPoly(..), defaultIntPolySizeLimits, IntPolySizeLimits(..))
import Numeric.AERN.Poly.IntPoly.Plot ()
import Numeric.AERN.Basics.Interval (Interval, IntervalApprox)

import Numeric.AERN.RmToRn 
        (getVarDoms, 
         newConstFn, newProjection,
         evaluationDefaultEffort)

import Numeric.AERN.RealArithmetic.Basis.Double ()
--import Numeric.AERN.RealArithmetic.Basis.MPFR

-- real arithmetic operators and imprecision measure:
import Numeric.AERN.RealArithmetic.RefinementOrderRounding 
    (RoundedReal, roundedRealDefaultEffort,
     expOutEff, expDefaultEffort)
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.ElementaryFromFieldOps.Exponentiation
        (ExpThinEffortIndicator(..))

import Numeric.AERN.RefinementOrder ((</\>))


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

---- End of imports -----


--type CF = Interval MPFR
type CF = Interval Double
type Poly = IntPoly String CF
type PIX = IntervalApprox Poly
type Fn = PIX

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
    fnGroups = [[fn1, fn2]]

    fn1Name = "0.2x + exp(x)"
    fn1 = pmDelta * x + expOutEff effExp x
    
    fn2Name = "(exp(x+-0.2))"
    fn2 = expOutEff effExp xpmDelta
        where
        xpmDelta = x + pmDelta
    
    pmDelta = newConstFn limits [("x", dom)] $ (- delta) </\> delta
    delta = 0.2
    
    x = newProjection limits [("x", dom)] "x"
    dom = (0) </\> 1
--    dom = (-0.2) </\> 0.2

    limits = 
        (defaultIntPolySizeLimits sampleCF cfLimits arity)
        {
            ipolylimits_maxdeg = maxdeg
        }
        where
        sampleCF = 0
        cfLimits = ()
        arity = 1

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
            [("fn1 ⊆ fn2 ?", functionInfos)]
        where
        sampleFn = x
        functionInfos =
            zip3 
                [fn1Name, fn2Name]
                [black, blue, blue] -- styles 
                (repeat True) -- show everything by default


plot :: ([[Fn]], FV.FnMetaData Fn) -> IO ()
plot (fns, fnmeta) =
    do
    -- enable multithreaded GUI:
    _ <- Gtk.unsafeInitGUIForThreadedRTS
    fnDataTV <- atomically $ newTVar $ FV.FnData $ addPlotVar fns
    fnMetaTV <- atomically $ newTVar $ fnmeta
    _ <- FV.new sampleFn effDrawFn effCF effEval (fnDataTV, fnMetaTV) Nothing
    Gtk.mainGUI
    where
    ((sampleFn :_) :_) = fns 
    effDrawFn = 
        (cairoDrawFnDefaultEffort sampleFn)
    effEval = 
        (evaluationDefaultEffort sampleFn)
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

