{-# LANGUAGE FlexibleContexts #-}

module Main where

-- numerical comparison abstraction and operators:
import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

-- real arithmetic operators and imprecision measure:
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

-- the MPFR interval type:
import qualified Numeric.AERN.MPFRBasis.Interval as MI

-- generic tools for controlling effort and formatting:
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.ShowInternals

import System.IO
import System.Environment

type RealApprox = MI.MI
type Precision = MI.Precision

main =
    do
    -- print a line asap:
    hSetBuffering stdout LineBuffering
    -- boilerplate to process arguments:
    [itersS, digitsS] <- getArgs
    let iters = read itersS
    let digits = read digitsS

    -- compute and print the result for each precision:
    mapM_ (reportItem digits) $ items iters digits
    where
    items iters digits =
        -- invoke an iRRAM-style procedure for automatic precision/effort incrementing: 
        withAccuracy maxIncrements (maxImprecision digits) initPrec $ 
            -- on the computation of iters-many iterations of the logistic map:
            \prec -> ((logistic prec r x0) !! (iters - 1))
    
    r :: Rational
    r = 40 / 10
    
    x0 :: RealApprox
    x0 = 0.671875

    maxImprecision :: Int -> RealApprox
    maxImprecision digits = (ensurePrecision 10 10)^^(-digits)

    initPrec :: Precision
    initPrec = 50
    
    maxIncrements = 100
    
    reportItem digits (prec, res) =    
        putStrLn $ formatRes prec res 
        where
        formatRes prec res =
            show prec ++ ": " 
            ++ (showInternals shouldShowInternals res) 
            ++ "; prec = " ++ (show $ imprecisionOfEff (imprecisionDefaultEffort res) res)
        shouldShowInternals = (digitsW+2, False)
        digitsW = fromIntegral digits
        
logistic :: 
    Precision -> 
    Rational {-^ scaling constant r -} -> 
    RealApprox {-^ initial value x_0 -} -> 
    [RealApprox]  {-^ sequence x_k defined by x_{k+1} = r*x_k*(1 - x_k) -}
logistic prec r x0 =
    iterate logisticAux $ ensurePrecision prec x0
    where
    logisticAux xPrev =
        r |<*> (xPrev * ((1::Int) |<+> (neg xPrev)))

ensurePrecision :: Precision -> RealApprox -> RealApprox
ensurePrecision prec x =
    (ArithInOut.convertOutEff prec (0:: Int)) <+> x 
        
{-|
    A generic function that applies a given approximate function
    repeatedly with increasing effort until the required accuracy
    is reached.
-}
withAccuracy :: 
    (HasImprecision t,
    NumOrd.PartialComparison (Imprecision t),
    EffortIndicator eff) 
    =>
    Int {-^ @iterLimit@ maximum number of different efforts to try out -} -> 
    Imprecision t {-^ @maxImprecision@ imprecision threshold for the result -} -> 
    eff {-^ @initEff@ the initial effort -} -> 
    (eff -> t) {-^ the function to compute -} -> 
    [(eff,t)] {-^ the efforts that were tried and the corresponding results -}
withAccuracy iterLimit maxImprecision initEff fn =
    stopWhenAccurate $ 
        take iterLimit $ 
            zip efforts (map fn efforts)
    where
    efforts = effortIncrementSequence initEff

    stopWhenAccurate [] = []
    stopWhenAccurate ((eff, result) : rest)
        | accurateEnough = [(eff, result)]
        | otherwise = (eff, result) : (stopWhenAccurate rest)
        where
        accurateEnough =
            (resultImprecision <=? maxImprecision) == Just True
        resultImprecision =
            imprecisionOfEff effImpr result
        effImpr =
            imprecisionDefaultEffort result
             