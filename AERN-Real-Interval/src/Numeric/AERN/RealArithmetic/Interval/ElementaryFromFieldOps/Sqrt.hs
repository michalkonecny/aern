{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps.Sqrt
    Description :  an interval-specific implementation of sqrt
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    An interval-specific implementation of sqrt.
-}

module Numeric.AERN.RealArithmetic.Interval.ElementaryFromFieldOps.Sqrt where

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.OpsImplicitEffort
--import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.Consistency
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Exception

import Control.Exception (throw)
import Control.Monad.ST (ST)

sqrtOutThinArg ::
    (HasZero e, HasOne e, Show e,
     NumOrd.RoundedLattice e,
     NumOrd.PartialComparison e,
     ArithUpDn.Convertible e Double,
     ArithUpDn.RoundedMixedField e Int,
     ArithUpDn.RoundedField e) =>
    ArithUpDn.FieldOpsEffortIndicator e ->
    ArithUpDn.MixedFieldOpsEffortIndicator e Int ->
    NumOrd.MinmaxEffortIndicator e ->
    NumOrd.PartialCompareEffortIndicator e ->
    ArithUpDn.ConvertEffortIndicator e Double ->
    Int {-^ the highest number of iterations of Newton method to make -} ->
    e {-^ @x@ as a singleton interval -} -> 
    (Interval e) {-^ @sqrt(x)@ -}
sqrtOutThinArg
        effortField
        effortMixedField
        effortMinmax
        effortCompare
        effortToDouble
        maxIters
        x
    | sureIsZero x = zero
    | not (sureAbove0 x) = 
        case (sureAbove0 (neg x)) of
            True -> 
                throw $ AERNDomViolationException $ 
                    "sqrtOutThinArg: applied to a negative argument " ++ show x
            _ ->
                throw $ AERNMaybeDomViolationException $ 
                    "sqrtOutThinArg: cannot check that sqrt is applied to a positive argument " ++ show x
    | xRecipSqrtDownInFastRegion =
--            unsafePrint ("AERN: sqrtOutThinArg: lower bound in fast region") $
        Interval 
            (x *. xRecipSqrtDown)
            (x *^ xRecipSqrtUp) -- best upper bound estimate based on an error estimate of the lower bound
    | sureAbove0 xRecipSqrtDown =
--            unsafePrint ("AERN: sqrtOutThinArg: lower bound NOT in fast region, using division") $
        Interval 
            (x *. xRecipSqrtDown)
            (recipUp xRecipSqrtDown) 
         -- an upper bound using division - introduces a fairly large error; used when iteration has not reached the fast region
    | otherwise =
--            unsafePrint ("AERN: sqrtOutThinArg: lower bound too close to zero, using dummy upper bound") $
        Interval
            (x *. xRecipSqrtDown)
            (NumOrd.maxUpEff effortMinmax x one)
         -- a dummy fallback upper bound where lower bound is too close to 0
    where
    (xRecipSqrtDownPrev, xRecipSqrtDown) = recipSqrtDown
    xRecipSqrtDownInFastRegion =
        case ArithUpDn.convertDnEff effortToDouble t of
            Just lowerBound -> lowerBound > (0.381966012 :: Double) -- (3 - sqrt 5)/2
            Nothing -> False
        where
        t = (xRecipSqrtDownPrev *. xRecipSqrtDownPrev) *. x
    xRecipSqrtUp = 
         -- only valid in "fast" region, ie where the error is smaller 
         -- than the gap between the results of the last two iterations
        (xRecipSqrtLastUp +^ xRecipSqrtLastUp) +^ (neg xRecipSqrtDownPrev)
    xRecipSqrtLastUp = 
            (xRecipSqrtDownPrev /^| 2 )
            *^
            (3 |+^ (neg $ x *. (xRecipSqrtDownPrev *. xRecipSqrtDownPrev))) 
            
    sureAbove0 t = 
        case ArithUpDn.convertDnEff effortToDouble t of
            Just lowerBound -> lowerBound > (0 :: Double)
            Nothing -> False
            
    sureIsZero t = 
        case NumOrd.pEqualEff effortCompare t zero of
            Just True -> True
            _ -> False
    
    x1 +^ x2 = ArithUpDn.addUpEff effortAdd x1 x2
    x1 *^ x2 = ArithUpDn.multUpEff effortMult x1 x2
    x1 *. x2 = ArithUpDn.multDnEff effortMult x1 x2
    recipUp x = ArithUpDn.recipUpEff effortDiv x
    recipDn x = ArithUpDn.recipDnEff effortDiv x
    n |+^ x = ArithUpDn.mixedAddUpEff effortAddInt x (n :: Int)
    n |+. x = ArithUpDn.mixedAddDnEff effortAddInt x  (n :: Int)
    x /^| n = ArithUpDn.mixedDivUpEff effortDivInt x  (n :: Int)
    x /.| n = ArithUpDn.mixedDivDnEff effortDivInt x  (n :: Int)
    
    effortAdd = ArithUpDn.fldEffortAdd x effortField
    effortMult = ArithUpDn.fldEffortMult x effortField
    effortDiv = ArithUpDn.fldEffortDiv x effortField
    effortAddInt = ArithUpDn.mxfldEffortAdd x (0::Int) effortMixedField
    effortDivInt = ArithUpDn.mxfldEffortDiv x (0::Int) effortMixedField

    recipSqrtDown
        | q0OK = -- computed an approximation in the stable region:
            iterRecipSqrt maxIters zero q0 -- then iterate!
        | otherwise = (zero, zero) -- zero is an always correct lower approximation
        where
        (q0OK, q0) = 
            (sureAbove0 xPlusOneUp && sureAbove0 babylon2, 
             recipDn babylon2)
            where
            -- babylon2 = (x+5)/4 - 1/(x+1) rounded upwards
            --   ie two Babylonian iterations
            --     \ t -> (t + x/t)/2 
            --   starting with t_0 = x:
            --   t_1 = (x + 1)/2
            --   t_2 = ((x + 1)/2 + x/((x + 1)/2))/2 =
            --         ((x + 1)/2 + 2x/(x + 1))/2 =
            --         ((x + 1)^2 + 4x)/4(x+1) =
            --         (x^2 + 6x + 1)/4(x+1) =
            --         (x^2 + 6x + 5 - 4)/4(x+1) =
            --         ((x + 5)(x + 1) - 4)/4(x+1) =
            --         (x + 5) - 1/(x+1)
            babylon2 = xPlus5Div4Up +^ (neg xPlusOneRecipDn)
            xPlus5Div4Up = ((5::Int) |+^ x) /^| (4::Int)
            xPlusOneRecipDn = recipDn xPlusOneUp
            xPlusOneUp = (1::Int) |+^ x
        -- iteratively improve q, a lower bound on sqrt(1/x)
        --   using the formula q_{n+1} = (q_n / 2) * (3 - x * q_n * q_n)  
        --   quoted eg in http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Iterative_methods_for_reciprocal_square_roots
        iterRecipSqrt maxIters qNm2 qNm1
            | maxIters > 0 && sureAbove0 qNm1 =
--                    unsafePrint ("AERN: sqrtOutThinArg: recipSqrtDown: iterRecipSqrt: maxIters = " ++ show maxIters) $
                iterRecipSqrt (maxIters - 1) qNm1 qN
            | otherwise = (qNm2, qNm1)
            where
            qN =
                (qNm1 /.| (2::Int))
                *.
                ((3::Int) |+. (neg $ x *^ (qNm1 *^ qNm1))) 


sqrtOutThinArgInPlace ::
    (CanBeMutable e,
     HasZero e, HasOne e, Show e,
     NumOrd.RoundedLattice e,
     NumOrd.RoundedLatticeInPlace e,
     NumOrd.PartialComparison e,
     ArithUpDn.Convertible e Double,
     ArithUpDn.RoundedMixedField e Int,
     ArithUpDn.RoundedMixedFieldInPlace e Int,
     ArithUpDn.RoundedField e,
     ArithUpDn.RoundedFieldInPlace e) =>
    ArithUpDn.FieldOpsEffortIndicator e ->
    ArithUpDn.MixedFieldOpsEffortIndicator e Int ->
    NumOrd.MinmaxEffortIndicator e ->
    NumOrd.PartialCompareEffortIndicator e ->
    ArithUpDn.ConvertEffortIndicator e Double ->
    Mutable (Interval e) s {-^ where to write the result @sqrt(x)@ -} ->
    Int {-^ the highest number of iterations of Newton method to make -} ->
    Mutable e s {-^ @x@ viewed as a singleton interval -} -> 
    ST s ()
sqrtOutThinArgInPlace
        effortField
        effortMixedField
        effortMinmax
        effortCompare
        effortToDouble
        resM@(MInterval resLM resRM)
        maxIters
        xM
    =
    do
    -- we need x - a pure version of xM for branching conditions:
    x <- unsafeReadMutable xM
    -- unsafe is OK because we do not write into xM and we write to resM only as the last thing
    --   and the value of x does not escape beyond this function
    let ?addUpDnEffort = ArithUpDn.fldEffortAdd x effortField
    let ?multUpDnEffort = ArithUpDn.fldEffortMult x effortField
    let ?mixedAddUpDnEffort = ArithUpDn.mxfldEffortAdd x (0::Int) effortMixedField
    let ?mixedMultUpDnEffort = ArithUpDn.mxfldEffortMult x (0::Int) effortMixedField
    let ?mixedDivUpDnEffort = ArithUpDn.mxfldEffortDiv x (0::Int) effortMixedField
    computeSqrt xM x
    where
    computeSqrt xM x =
        do
        continue
        where
        continue
            | sureIsZero x = 
                writeMutable resM zero
            | not (sureAbove0 x) = 
                case (sureAbove0 (neg x)) of
                    True -> 
                        throw $ AERNDomViolationException $ 
                            "sqrtOutThinArgInPlace: applied to a negative argument " ++ show x
                    _ ->
                        throw $ AERNMaybeDomViolationException $ 
                            "sqrtOutThinArgInPlace: cannot check that sqrt is applied to a positive argument " ++ show x
            | otherwise =
                do
                -- declare some variables:
                temp1M <- makeMutable zero
                temp2M <- makeMutable zero
                -- iterate using Newton's method, assign results of last two iterations to the above vars: 
                prevFirst <- recipSqrtDown temp1M temp2M
                case prevFirst of
                    True -> assignBounds temp1M temp2M
                    False -> assignBounds temp2M temp1M
                where
                assignBounds xRecipSqrtDownPrevM xRecipSqrtDownM =
                    do
                    xRecipSqrtDown <- unsafeReadMutable xRecipSqrtDownM
                    xRecipSqrtDownPrev <- unsafeReadMutable xRecipSqrtDownPrevM
                    -- assign lower bound resL := x *. xRecipSqrtDown:
                    ArithUpDn.multDnInPlaceEff effortMult resLM xM xRecipSqrtDownM
                    -- assign upper bound the best applicable method out of three methods: 
                    constructUpperBound xRecipSqrtDownM xRecipSqrtDown xRecipSqrtDownPrevM xRecipSqrtDownPrev
                constructUpperBound xRecipSqrtDownM xRecipSqrtDown xRecipSqrtDownPrevM xRecipSqrtDownPrev 
                    | xRecipSqrtDownInFastRegion =
                        do
                        -- in fast region, use the difference between the last two approx:
    
                        -- first, we need an upwards-rounded version of xRecipSqrtDown:                  
                        -- xRecipSqrtLastUp := newton... xRecipSqrtDownPrev:
                        let xRecipSqrtLastUpM = xRecipSqrtDownM -- safely reuse variable
                        newtonIterateUp xRecipSqrtLastUpM xRecipSqrtDownPrevM
                        
                        -- now compute and use the difference:
                        -- xRecipSqrtUp := 2*^xRecipSqrtLastUp -^ xRecipSqrtDownPrev:
                        --   only valid in "fast" region, ie where the error is smaller 
                        --   than the gap between the results of the last two iterations
                        let xRecipSqrtUpM = xRecipSqrtLastUpM -- safely reuse variable
                        xRecipSqrtLastUpM *^|= (2 :: Int)
                        xRecipSqrtLastUpM -^= xRecipSqrtDownPrevM
                        
                        -- assign upper bound resL := x *. xRecipSqrtUp:
                        ArithUpDn.multUpInPlaceEff effortMult resRM xM xRecipSqrtUpM
                    | sureAbove0 xRecipSqrtDown =
                        do
                        -- compute upper bound resR := 1 /^ xRecipSqrtDown:
                        --   introduces a fairly large error; 
                        --   used when iteration has not reached the fast region
                        ArithUpDn.recipUpInPlaceEff effortDiv resRM xRecipSqrtDownM
                    | otherwise =
                        do
                        -- a dummy fallback upper bound where lower bound is too close to 0:
                        unsafeWriteMutable resRM $
                            NumOrd.maxUpEff effortMinmax x one
                    where
                    xRecipSqrtDownInFastRegion =
                        case ArithUpDn.convertDnEff effortToDouble t of
                            Just lowerBound -> lowerBound > (0.381966012 :: Double) -- (3 - sqrt 5)/2
                            Nothing -> False
                        where
                        t = 
                            (xRecipSqrtDownPrev *. xRecipSqrtDownPrev) *. x
                newtonIterateUp resM tM = 
                    -- assumes no aliasing between resM and tM, does not change tM
                    do
                    -- res :=
                    --    (t /^| 2 )
                    --     *^
                    --    (3 |+^ (neg $ x *. (t *. t))) 
                    ArithUpDn.multUpInPlaceEff effortMult resM tM tM
                    resM *^= xM
                    negInPlace resM resM
                    resM +.|= (3 :: Int)
                    resM *.= tM
                    resM /.|= (2 :: Int)
    
                newtonIterateDn resM tM = 
                    -- assumes no aliasing between resM and tM, does not change tM
                    do
                    -- res :=
                    --    (t /.| 2 )
                    --     *.
                    --    (3 |+. (neg $ x *^ (t *^ t))) 
                    ArithUpDn.multDnInPlaceEff effortMult resM tM tM
                    resM *.= xM
                    negInPlace resM resM
                    resM +^|= (3 :: Int)
                    resM *^= tM
                    resM /^|= (2 :: Int)
    
                sureAbove0 t = 
                    case ArithUpDn.convertDnEff effortToDouble t of
                        Just lowerBound -> lowerBound > (0 :: Double)
                        Nothing -> False
                        
                sureIsZero t = 
                    case NumOrd.pEqualEff effortCompare t zero of
                        Just True -> True
                        _ -> False
                
                effortAdd = ArithUpDn.fldEffortAdd x effortField
                effortMult = ArithUpDn.fldEffortMult x effortField
                effortDiv = ArithUpDn.fldEffortDiv x effortField
                effortMultInt = ArithUpDn.mxfldEffortMult x (0::Int) effortMixedField
                effortAddInt = ArithUpDn.mxfldEffortAdd x (0::Int) effortMixedField
                effortDivInt = ArithUpDn.mxfldEffortDiv x (0::Int) effortMixedField
            
                recipSqrtDown aM bM
                    | q0OK = -- computed an approximation in the stable region:
                        do
                        writeMutable aM zero
                        writeMutable bM q0
                        iterRecipSqrt maxIters True aM bM -- then iterate!
                    | otherwise = 
                        do
                        writeMutable aM zero -- zero is an always correct lower approximation
                        writeMutable bM zero
                        return True
                    where
                    (q0OK, q0) = 
                        (sureAbove0 xPlusOneUp && sureAbove0 babylon2, 
                         recipDn babylon2)
                        where
                        -- babylon2 = (x+5)/4 - 1/(x+1) rounded upwards
                        --   ie two Babylonian iterations
                        --     \ t -> (t + x/t)/2 
                        --   starting with t_0 = x:
                        --   t_1 = (x + 1)/2
                        --   t_2 = ((x + 1)/2 + x/((x + 1)/2))/2 =
                        --         ((x + 1)/2 + 2x/(x + 1))/2 =
                        --         ((x + 1)^2 + 4x)/4(x+1) =
                        --         (x^2 + 6x + 1)/4(x+1) =
                        --         (x^2 + 6x + 5 - 4)/4(x+1) =
                        --         ((x + 5)(x + 1) - 4)/4(x+1) =
                        --         (x + 5) - 1/(x+1)
                        babylon2
                            = xPlus5Div4Up +^ (neg xPlusOneRecipDn)
                        xPlus5Div4Up 
                            = ((5::Int) |+^ x) /^| (4::Int)
                        xPlusOneRecipDn = recipDn xPlusOneUp
                        xPlusOneUp 
                            = (1::Int) |+^ x
                        recipDn = ArithUpDn.recipDnEff effortDiv
                    -- iteratively improve q, a lower bound on sqrt(1/x)
                    --   using the formula q_{n+1} = (q_n / 2) * (3 - x * q_n * q_n)  
                    --   quoted eg in http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Iterative_methods_for_reciprocal_square_roots
                    iterRecipSqrt maxIters prevFirst aM bM =
                        -- assuming aM and bM do not alias
                        do
                        qNm1 <- unsafeReadMutable qNm1M
                        case maxIters > 0 && sureAbove0 qNm1 of
                            False -> -- should not or cannot continue iterating
                                return prevFirst -- indicate which of the two variables has the older result
                            True ->
                                do
                                newtonIterateDn qN qNm1M
                                iterRecipSqrt (maxIters - 1) (not prevFirst) bM aM -- swap the variables
                        where
                        (qNm2M, qNm1M) 
                            | prevFirst = (aM,bM)
                            | otherwise = (bM,aM)
                        qN = qNm2M