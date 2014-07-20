module Main where

-- the MPFR interval type:
import qualified Numeric.AERN.MPFRBasis.Interval as MI

-- real arithmetic operators and imprecision measure:
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Operators ((|<*>))
import Numeric.AERN.RealArithmetic.RefinementOrderRounding (convertOutEff)
import Numeric.AERN.RealArithmetic.Measures (imprecisionOf, iterateUntilAccurate)

-- ability to change precision of numbers:
import Numeric.AERN.Basics.SizeLimits (changeSizeLimitsOut)

-- generic tools for controlling formatting:
import Numeric.AERN.Basics.ShowInternals (showInternals)


import System.IO
import System.Environment

type RealApprox = MI.MI -- MPFR interval
type Precision = MI.Precision

usage :: String
usage = "Usage: LogisticMap <number of iterations> <result digits>"

processArgs :: [String] -> (Int, Int)
processArgs [itersS, digitsS] =
    case (reads itersS, reads digitsS) of
        ((iters, []):_,(digits, []):_) -> (iters, digits)
        _ -> error $ usage
processArgs _ = error $ usage

main =
    do
    -- set console to print each line asap:
    hSetBuffering stdout LineBuffering
    -- process command line arguments:
    args <- getArgs
    let (iters, digits) = processArgs args 

    -- compute and print the result for each precision:
    mapM_ (reportItem digits) $ items iters digits
    
    where
    items iters digits =
        -- invoke an iRRAM-style procedure for automatic precision/effort incrementing 
        --    on the computation of iters-many iterations of the logistic map:
        (iterateUntilAccurate initPrec maxAttempts maxImprecision) 
            logisticMapWithPrec
        where
        initPrec = 50 -- try with the precision first
        maxAttempts = 100 -- try to increase precision 100 times before giving up
        maxImprecision = (ensurePrecision 1000 10)^^(-digits) -- target result precision
        
        logisticMapWithPrec prec = 
            -- iterate logistic map iters times, using precision prec
            (iterate (logisticMap r) x0Prec) !! (iters - 1)
            where
            x0Prec = ensurePrecision prec x0
    
            r :: Rational
            r = 375 / 100
            
            x0 :: RealApprox
            x0 = 0.5 -- 0.671875
        
    
    reportItem digits (prec, res) =    
        putStrLn $ formatRes prec res 
        where
        formatRes prec res =
            show prec ++ ": " 
            ++ (showInternals shouldShowInternals res) 
            ++ "; prec = " ++ (show $ imprecisionOf res)
        shouldShowInternals = (digitsW+2, False)
        digitsW = fromIntegral digits        
       

logisticMap ::
    Rational {-^ scaling constant r -} -> 
    RealApprox {-^ previous value x_0 -} ->
    RealApprox {-^ result -} 
logisticMap r xPrev =
    r |<*> (xPrev * (1 - xPrev))
    -- The operator |<*> stands for mixed-type outwards-rounded multiplication.
    -- The <> surrounding the operator * indicate outwards rounding.
    -- The | preceding the operator <*> indicate that the type of the first
    -- operand is different than the type of the second operand and the result.

ensurePrecision :: Precision -> RealApprox -> RealApprox
ensurePrecision prec x =
    changeSizeLimitsOut prec x
        
             