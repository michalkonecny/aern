{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Evaluation
    Description :  evaluation of interval polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Evaluation of interval polynomials in general using its instance of 'CanEvaluateOtherType' 
    and specilised cases for evaluation at a point and on an interval.
-}

module Numeric.AERN.Poly.IntPoly.Evaluation
    (
        PolyEvalOps(..),
        PolyEvalMonoOps(..),
        evalPolyAtPointOut,
        evalPolyAtPointIn,
        evalPolyOnIntervalOut,
        evalPolyOnIntervalIn
    )
where

import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.New ()
import Numeric.AERN.Poly.IntPoly.Differentiation

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

--import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

--import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap

instance 
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf)
    =>
    CanEvaluateOtherType (IntPoly var cf)
    where
    type EvalOps (IntPoly var cf) =  PolyEvalOps var cf
    evalOtherType ops valsMap p@(IntPoly cfg _) =
        case polyEvalMonoOps ops of
            Nothing -> evalPolyDirect ops vals p
            _ -> evalPolyMono ops vals p 
        where
        vals = valsMapToValues cfg valsMap

valsMapToValues :: 
    (Ord var) 
     =>
     IntPolyCfg var cf -> 
     (VarBox (IntPoly var cf) t) -> 
     [t]
valsMapToValues cfg valsMap =
    vals
    where
    vals = map getValue vars
    vars = ipolycfg_vars cfg
    getValue var = 
        case lookupVar valsMap var of 
            Just val -> val
            _ -> error "aern-poly internal error in Evaluation...valsMapToValues"
     
instance
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedReal cf, 
     HasAntiConsistency cf, 
     RefOrd.IntervalLike cf)
    =>
    CanEvaluate (IntPoly var cf)
    where
    type (EvaluationEffortIndicator (IntPoly var cf)) = 
        ArithInOut.RoundedRealEffortIndicator cf
    evaluationDefaultEffort (IntPoly cfg _) =
        ArithInOut.roundedRealDefaultEffort (ipolycfg_sample_cf cfg)
    evalAtPointOutEff eff valsMap p@(IntPoly cfg _) 
        | valuesAreExact values =
            evalPolyAtPointOut eff values p
        | otherwise =
            evalPolyOnIntervalOut eff values p
        where
        values = valsMapToValues cfg valsMap
    evalAtPointInEff eff valsMap p@(IntPoly cfg _) 
        | valuesAreExact values =
            evalPolyAtPointIn eff values p
        | otherwise =
            evalPolyOnIntervalIn eff values p
        where
        values = valsMapToValues cfg valsMap
    
valuesAreExact :: 
    HasImprecision t 
    => 
    [t] -> Bool
valuesAreExact values =
    and $ map isCertainlyExact values
    where
    isCertainlyExact val = 
        isExactEff (imprecisionDefaultEffort val) val == Just True 

    
instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, Show cf)
    =>
    HasEvalOps (IntPoly var cf) cf
    where
    type EvalOpsEffortIndicator (IntPoly var cf) cf = ArithInOut.RoundedRealEffortIndicator cf
    evalOpsDefaultEffort _ sampleCf = ArithInOut.roundedRealDefaultEffort sampleCf 
    evalOpsIn _eff _sampleP _sampleCf =
        error "aern-poly: IntPoly operators for evaluation at a subdomain: inner rounding not available"
    evalOpsOut eff _sampleP sampleCf =
        coeffPolyEvalOpsOut eff sampleCf

data PolyEvalOps var cf val =
    PolyEvalOps
    {
        polyEvalZero :: val,
        polyEvalAdd :: (val -> val -> val),
        polyEvalMult :: (val -> val -> val),
        polyEvalPow :: (val -> Int -> val), {-^ non-negative integer power -}
        polyEvalCoeff :: (cf -> val), {-^ coeff conversion -}
        polyEvalMaybePoly :: (IntPolyTerms var cf -> Maybe val), {-^ optional direct poly conversion -}
        polyEvalLeq :: (val -> val -> Maybe Bool),
        polyEvalMonoOps :: Maybe (PolyEvalMonoOps cf val)
    }

data PolyEvalMonoOps cf val =
    PolyEvalMonoOps
    {
        polyEvalMonoGetEndpoints :: val -> (val, val),
        polyEvalMonoFromEndpoints :: (val, val) -> val,
        polyEvalMonoIsThin :: val -> Bool,
        polyEvalMonoCfEffortIndicator :: ArithInOut.RoundedRealEffortIndicator cf
    }


coeffPolyEvalOpsOut ::
    (RefOrd.IntervalLike cf, ArithInOut.RoundedReal cf)
    =>
   (ArithInOut.RoundedRealEffortIndicator cf) ->
   cf ->
   PolyEvalOps var cf cf
coeffPolyEvalOpsOut eff sample =
    let ?multInOutEffort = effMult in
    let ?intPowerInOutEffort = effPwr in
    let ?addInOutEffort = effAdd in
    let ?pCompareEffort = effComp in
    PolyEvalOps (zero sample) (<+>) (<*>) (<^>) id (const Nothing) (<=?) $
        Just $ PolyEvalMonoOps
            RefOrd.getEndpointsOutWithDefaultEffort
            RefOrd.fromEndpointsOutWithDefaultEffort
            isDefinitelyExact
            eff
    where
    isDefinitelyExact a =
        (isExactEff $ ArithInOut.rrEffortImprecision a eff) a == Just True
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    effComp = ArithInOut.rrEffortNumComp sample eff
--    effJoin = ArithInOut.rrEffortJoinMeet sample eff


instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, Show cf)
    =>
    ArithUpDn.Convertible (IntPoly var cf) cf
    where
    type ArithUpDn.ConvertEffortIndicator (IntPoly var cf) cf = 
        (ArithInOut.RoundedRealEffortIndicator cf, RefOrd.GetEndpointsEffortIndicator cf)
    convertDefaultEffort _sampleP sampleCf = 
        (ArithInOut.roundedRealDefaultEffort sampleCf, RefOrd.getEndpointsDefaultEffort sampleCf)
    convertUpEff (effCf, effGetEndpts) p =
        Just $ snd $ RefOrd.getEndpointsOutEff effGetEndpts range
        where
        range = evalOtherType (evalOpsOut effCf sampleP sampleCf) varDoms p 
        sampleP = p
        sampleCf = getSampleDomValue sampleP
        varDoms = getDomainBox p
    convertDnEff (effCf, effGetEndpts) p =
        Just $ fst $ RefOrd.getEndpointsOutEff effGetEndpts range
        where
        range = evalOtherType (evalOpsOut effCf sampleP sampleCf) varDoms p 
        sampleP = p
        
        sampleCf = getSampleDomValue sampleP
        varDoms = getDomainBox p
    

evalPolyAtPointOut, evalPolyAtPointIn ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     HasAntiConsistency cf, 
     Show cf) 
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    [cf] {- values for each variable respectively -} -> 
    IntPoly var cf -> cf
evalPolyAtPointOut eff values p@(IntPoly cfg _)
    = 
    evalPolyDirect (coeffPolyEvalOpsOut eff sample) values p
    where
    sample = ipolycfg_sample_cf cfg
evalPolyAtPointIn eff values p@(IntPoly cfg _)
    = 
    flipConsistency $
        evalPolyDirect (coeffPolyEvalOpsOut eff sample) values $ 
            flipConsistencyPoly p
    where
    sample = ipolycfg_sample_cf cfg

evalPolyOnIntervalOut, evalPolyOnIntervalIn ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, 
     HasAntiConsistency cf, 
     Show cf) 
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    [cf] {- values for each variable respectively -} -> 
    IntPoly var cf -> cf
evalPolyOnIntervalOut eff values p@(IntPoly cfg _)
    = 
    evalPolyMono (coeffPolyEvalOpsOut eff sample) values p
    where
    sample = ipolycfg_sample_cf cfg
evalPolyOnIntervalIn eff values p@(IntPoly cfg _)
    = 
    flipConsistency $
        evalPolyMono (coeffPolyEvalOpsOut eff sample) values $ 
            flipConsistencyPoly p
    where
    sample = ipolycfg_sample_cf cfg

evalPolyDirect ::
    (Ord var, Show var, Show cf, Show val, Neg cf) => 
    (PolyEvalOps var cf val) ->
    [val] ->
    IntPoly var cf -> val
evalPolyDirect opsV values _p@(IntPoly cfg terms)
    = 
--    unsafePrint
--    (
--        "evalPolyDirect: "
--        ++ "\n  values = " ++ show values
--        ++ "\n  p = " ++ show p
--    ) $
    ev values domsLE terms
    where
    zV = polyEvalZero opsV
    addV = polyEvalAdd opsV 
    multV = polyEvalMult opsV
    powV = polyEvalPow opsV
    cfV = polyEvalCoeff opsV
    polyV = polyEvalMaybePoly opsV
    domsLE = ipolycfg_domsLE cfg
    ev [] [] (IntPolyC cf) = cfV cf
    ev (varValue : restVars) (domLE : restDoms) (IntPolyV _ polys)
        | IntMap.null polys = zV
        | lowestExponent == 0 = 
            resultMaybeWithoutConstantTerm
        | otherwise =
            (varValueLZ `powV` lowestExponent) `multV` resultMaybeWithoutConstantTerm 
        where
        varValueLZ =
            addV varValue (cfV $ neg domLE)
        (lowestExponent, resultMaybeWithoutConstantTerm) 
            = IntMap.foldWithKey addTerm (highestExponent, zV) polys 
        (highestExponent, _) = IntMap.findMax polys 
        addTerm exponent_2 poly (prevExponent, prevVal) = (exponent_2, newVal)
            where
            newVal = -- Horner scheme:
                polyValue 
                `addV`
                (prevVal `multV` (varValueLZ `powV` (prevExponent - exponent_2)))
            polyValue =
                case polyV poly of
                    Just value -> value
                    Nothing -> ev restVars restDoms poly  
    ev varVals domsLE_2 terms_2 =
        error $ 
            "evalPolyDirect: illegal case:" 
            ++ "\n varVals = " ++ show varVals 
            ++ "\n domsLE = " ++ show domsLE_2 
            ++ "\n terms = " ++ show terms_2

-- TODO: make the following function generic for any function representation with nominal derivatives    
evalPolyMono ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, 
     RefOrd.IntervalLike cf,
     Show cf, Show val) 
    => 
    (PolyEvalOps var cf val) ->
    [val] {-^ value for each variable -} -> 
    IntPoly var cf -> val
evalPolyMono opsV values p@(IntPoly cfg _)
    | noMonoOps = direct
    | noMonotoneVar = direct
    | otherwise =
--        unsafePrint
--        (
--            "evalPolyMono: "
--            ++ "\n p = " ++ show p
--            ++ "\n values = " ++ show values
--            ++ "\n valuesL = " ++ show valuesL
--            ++ "\n valuesR = " ++ show valuesR
--            ++ "\n left = " ++ show left
--            ++ "\n right = " ++ show right
--            ++ "\n direct = " ++ show direct
--        ) $ 
        fromEndPtsV (left, right)
    where
    direct = evalPolyDirect opsV values p
    zV = polyEvalZero opsV
--    addV = polyEvalAdd opsV 
--    multV = polyEvalMult opsV
--    powV = polyEvalPow opsV
--    cfV = polyEvalCoeff opsV
--    polyV = polyEvalMaybePoly opsV
    leqV = polyEvalLeq opsV
    (noMonoOps, monoOpsV) = 
        case polyEvalMonoOps opsV of
            Nothing -> (True, error "evalPolyMono: internal error: monoOpsV used when not present")
            Just monoOpsV_2 -> (False, monoOpsV_2)
    fromEndPtsV = polyEvalMonoFromEndpoints monoOpsV
    getEndPtsV = polyEvalMonoGetEndpoints monoOpsV
    isExact = polyEvalMonoIsThin monoOpsV
    eff= polyEvalMonoCfEffortIndicator monoOpsV
    left = evalPolyDirect opsV valuesL p
    right = evalPolyDirect opsV valuesR p
    (noMonotoneVar, valuesL, valuesR) =
        let ?mixedMultInOutEffort = effMult in
        detectMono True [] [] $ reverse $ zip vars $ values -- undo reverse due to the accummulators
    vars = ipolycfg_vars cfg
    detectMono noMonotoneVarPrev valuesLPrev valuesRPrev []
        = (noMonotoneVarPrev, valuesLPrev, valuesRPrev)
    detectMono noMonotoneVarPrev valuesLPrev valuesRPrev ((var, val) : rest)
        =
--        unsafePrint 
--        (
--            "evalPolyMono: detectMono: deriv = " ++ show deriv
--        ) $ 
        detectMono noMonotoneVarNew (valLNew : valuesLPrev) (valRNew : valuesRPrev) rest
        where
        noMonotoneVarNew = noMonotoneVarPrev && varNotMono
        (valLNew, valRNew)
            | varNotMono = (val, val) -- not monotone, we have to be safe
            | varNonDecr = (valL, valR) -- non-decreasing on the whole domain - can use endpoints
            | otherwise = (valR, valL) -- non-increasing on the whole domain - can use swapped endpoints
        (varNonDecr, varNotMono) =
            case (valIsExact, zV `leqV` deriv, deriv `leqV` zV) of
                (True, _, _) -> (undefined, True) -- when a variable has a thin domain, do not bother separating endpoints 
                (_, Just True, _) -> (True, False) 
                (_, _, Just True) -> (False, False)
                _ -> (undefined, True)
        deriv =
            evalPolyDirect opsV values $ 
                diffPolyOut eff var p -- range of (d p)/(d var)    
        (valL, valR) = getEndPtsV val
        valIsExact = isExact val
    effMult = ArithInOut.mxfldEffortMult sampleCf (1::Int) $ ArithInOut.rrEffortIntMixedField sampleCf eff
    sampleCf = ipolycfg_sample_cf cfg
        