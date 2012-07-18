module Main where

-- the MPFR interval type:
import qualified Numeric.AERN.MPFRBasis.Interval as MI

-- numerical comparison abstraction and operators:
import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

-- real arithmetic operators and imprecision measure:
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

-- generic tools for controlling effort and formatting:
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.ShowInternals

import System.IO
import System.Environment

type RealApprox = MI.MI
type Precision = MI.Precision

main =
    do
    -- print each line asap:
    hSetBuffering stdout LineBuffering
    -- boilerplate to process arguments:
    [itersS, digitsS] <- getArgs
    let iters = read itersS -- number of iterations of the logistic map
    let digits = read digitsS -- desired accuracy in decimal digits

    -- compute and print the result for each precision:
    mapM_ (reportItem digits) $ items iters digits
    where
    items iters digits =
        -- invoke an iRRAM-style procedure for automatic precision/effort incrementing: 
        iterateUntilAccurate maxIncrements (maxImprecision digits) initPrec $ 
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
        
             