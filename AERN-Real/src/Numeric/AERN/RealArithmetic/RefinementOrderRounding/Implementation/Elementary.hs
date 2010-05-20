{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.Implementation.Elementary
    Description :  implementation of in/out rounded elementary operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Implementation of in/out rounded elementary operations.
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.Implementation.Elementary where

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.ExactOps


expOutThinArg 
        (effortAddInt, effortMult, effortDivInt) 
        (effortMeet, effortFromDbl, effortToInt) 
        (Int1To100 degr) x
    | not (excludesPlusInfinity x) = -- ie x = oo
        x 
    | not (excludesMinusInfinity x) = -- ie x = -oo
--        unsafePrintReturn
--        (
--            "expOutNearZero (x allows -infty): "
--            ++ "\n x = " ++ show x
--            ++ "\n effort = " ++ show effort
--            ++ "\n effortTaylor = " ++ show effortTaylor
--            ++ "\n result = "
--        ) $
        zero -- infinities not handled well by the Taylor formula
    | otherwise =
        let ?addMixInOutEffort = effortAddInt in
        let ?divMixInOutEffort = effortDivInt in
        let ?multInOutEffort = effortMult in
        let ?joinmeetInOutEffort = effortMeet in
        expOutNearZero effortFromDbl effortToInt degr x
   -- TODO: adapt code of erExp_R
   -- deal with infinities, scale near 0, then scale back
   
{-|
    A Taylor series for exponentiation, assuming the parameter is an exact 
    approximation and "near" zero, in particular, not infinite.    
-}
expOutNearZero effortFromDbl effortToInt degree x =
    x
--        unsafePrintReturn
--        (
--            "expOutNearZero (x excludes -infty): "
--            ++ "\n x = " ++ show x
--            ++ "\n effort = " ++ show effort
--            ++ "\n effortTaylor = " ++ show effortTaylor
--            ++ "\n result = "
--        ) $
--        1 <|+> (te degree 1)
--    where
--    te steps i
--        | steps > 0 =
--            (x </|> i) <*> (1 <|+> (te (steps - 1) (i + 1)))
--        | steps == 0 = 
--            errorBound
--            where
--            errorBound = 
--                (x </|> i) <*> ithDerivBound
--            ithDerivBound
--                | xCeiling < 0 = -- certainly negative:
--                    pow26xFloor <|/\> one
--                | xFloor > 0 = -- certainly positive:
--                    one <|/\> pow28xCeiling
--                | otherwise = -- could contain 0:
--                    pow26xFloor <|/\> pow28xCeiling
--    xCeiling = ArithUpDn.toIntegerUpEff effortToInt x
--    xFloor = ArithUpDn.toIntegerDnEff effortToInt x
--    pow26xFloor =
--        (ArithInOut.fromDoubleOutEff effortFromDbl 2.6) <^^> xFloor 
--                    -- lower estimate of e^x
--    pow28xCeiling = 
--        (ArithInOut.fromDoubleOutEff effortFromDbl 2.8) <^^> xCeiling 
--                    -- upper estimate of e^x
--   