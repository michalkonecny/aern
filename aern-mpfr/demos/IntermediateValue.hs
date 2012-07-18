{-# LANGUAGE FlexibleContexts #-}

module Main where

-- the MPFR interval type:
import qualified Numeric.AERN.MPFRBasis.Interval as MI

-- numerical comparison abstraction and operators:
import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsDefaultEffort

-- refinement order abstraction and operators:
import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsDefaultEffort

-- real arithmetic operators and imprecision measure:
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

-- generic tools for controlling effort and formatting:
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.ShowInternals

import Data.Maybe (catMaybes)

import System.IO
import System.Environment

type RealApprox = MI.MI
type Precision = MI.Precision

main =
    do
    -- print a line asap:
    hSetBuffering stdout LineBuffering
    
    -- sqrt(2) with fixed effort:
    putStrLn $ "findRoot(fn(x)=x^2-2, leftEndpoint=0, rightEndpoint=2, prec=100, effFn = ()) = "
    putStrLn $ show $ findRoot (100,()) (\_ x -> x * x - 2) 0 2
    
    -- log(2) with fixed effort:
    putStrLn $ "findRoot(fn(x)=exp(x)-2, leftEndpoint=0, rightEndpoint=1, prec=100, effFn = ()) = "
    putStrLn $ show $ findRoot (100, 100) (\eff x -> (ArithInOut.expOutEff eff x) - 2) 0 1

findRoot ::
    (Precision, effort) ->
    (effort -> RealApprox -> RealApprox) ->
    RealApprox ->
    RealApprox ->
    RealApprox
findRoot (prec, effFn) fn leftEndpoint rightEndpoint =
    case oppositeSigns leftSign rightSign of
        True ->
            shrinkLRandMerge (leftEndpointPrec, leftSign)  (rightEndpointPrec, rightSign)
        False ->
            error $
                "findRoot: cannot establish that the given function fn has opposite signs at the endpoints:"
                ++ "\n fn(" ++ show leftEndpointPrec ++ ") = " ++ show (fn effFn leftEndpointPrec)
                ++ "\n fn(" ++ show rightEndpointPrec ++ ") = " ++ show (fn effFn rightEndpointPrec)
    where
    leftSign = getSign leftEndpointPrec
    rightSign = getSign rightEndpointPrec
    
    leftEndpointPrec = ensurePrecision prec leftEndpoint
    rightEndpointPrec = ensurePrecision prec rightEndpoint
    
    oppositeSigns (Just Positive) (Just Negative) = True
    oppositeSigns (Just Negative) (Just Positive) = True
    oppositeSigns _ _ = False
    
    shrinkLRandMerge lAndSign@(l,lSign) rAndSign@(r,rSign) =
        case findPointWithSign l r of
            Nothing -> l </\> r
            Just (mAndSign@(_, mSign))
                | oppositeSigns lSign mSign -> shrinkLRandMerge lAndSign mAndSign
                | oppositeSigns mSign rSign -> shrinkLRandMerge mAndSign rAndSign
    findPointWithSign l r =
        case potentialSplitPoints of
            [] -> Nothing
            mAndSign : _ -> Just mAndSign 
        where
        potentialSplitPoints = 
            filter hasSign $ map addSign $ pointsInBetween l r
        addSign x = (x, getSign x)
        hasSign (_, Nothing) = False
        hasSign _ = True
    getSign x =
        case (fnAtX >? 0, fnAtX <? 0) of
            (Just True, _) -> Just Positive
            (_, Just True) -> Just Negative
            _ -> Nothing
        where
        fnAtX = fn effFn x
    pointsInBetween l r =
--        filter betweenLR $
--        map getLeftEndpoint $
        [
            (l * 2 + r) / 3
        ,
            (l + r * 2) / 3
        ]
--        where
--        betweenLR x =
--            ((l <? x) == Just True)
--            &&
--            ((x <? r) == Just True)
--        getLeftEndpoint x =
--            fst $ RefOrd.getEndpointsOutWithDefaultEffort x
    
data Sign = Positive | Negative

ensurePrecision :: Precision -> RealApprox -> RealApprox
ensurePrecision prec x =
    (ArithInOut.convertOutEff prec (0:: Int)) <+> x 
        

--expEffort :: Int -> ArithInOut.ExpEffortIndicator RealApprox
--expEffort n =
--    (a, Int1To10 n)
--    where
--    (a, _) = ArithInOut.expDefaultEffort a

   
             