{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluation
    Description :  evaluation of interval polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Evaluation of interval polynomials.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluation
--    (
--    )
where

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Differentiate

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.Evaluation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures

import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort
import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsImplicitEffort

import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap

instance 
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, Show cf)
    =>
    (CanEvaluateOtherType (IntPoly var cf))
    where
    type EvalOps (IntPoly var cf) =  PolyEvalOps var cf
    evalOtherType ops valsMap p@(IntPoly cfg _) =
        case polyEvalMonoOps ops of
            Nothing -> evalPolyDirect ops vals p
            _ -> evalPolyMono ops vals p 
        where
        vals = map getValue vars
        vars = ipolycfg_vars cfg
        getValue var = case lookupVar valsMap var of Just val -> val

instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, Show cf)
    =>
    HasEvalOps (IntPoly var cf) cf
    where
    type EvalOpsEffortIndicator (IntPoly var cf) cf = ArithInOut.RoundedRealEffortIndicator cf
    evalOpsDefaultEffort _ sampleCf = ArithInOut.roundedRealDefaultEffort sampleCf 
    evalOpsOut eff sampleP sampleCf = 
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
    let ?joinmeetEffort = effJoin in
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
    effJoin = ArithInOut.rrEffortJoinMeet sample eff


instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, Show cf)
    =>
    ArithUpDn.Convertible (IntPoly var cf) cf
    where
    type ArithUpDn.ConvertEffortIndicator (IntPoly var cf) cf = 
        (ArithInOut.RoundedRealEffortIndicator cf, RefOrd.GetEndpointsEffortIndicator cf)
    convertDefaultEffort sampleP sampleCf = 
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
    

evalPolyAtPoint ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, Show cf) 
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    [cf] {- values for each variable respectively -} -> 
    IntPoly var cf -> cf
evalPolyAtPoint eff values p@(IntPoly cfg _)
    = 
    evalPolyDirect (coeffPolyEvalOpsOut eff sample) values p
    where
    sample = ipolycfg_sample_cf cfg

evalPolyOnInterval ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, 
     Show cf) 
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    [cf] {- values for each variable respectively -} -> 
    IntPoly var cf -> cf
evalPolyOnInterval eff values p@(IntPoly cfg _)
    = 
    evalPolyMono (coeffPolyEvalOpsOut eff sample)  values p
    where
    valuesLR = map RefOrd.getEndpointsOutWithDefaultEffort values
    valuesALR =
        let ?pCompareEffort = effComp in
        zip values $ map checkEqual valuesLR
        where
        values = 
            let ?joinmeetEffort = effJoin in
            map (uncurry (</\>)) valuesLR
        checkEqual (l,r) 
            | (l ==? r) == Just True = Nothing
            | otherwise = Just (l,r)
    effComp = ArithInOut.rrEffortNumComp sample eff
    effJoin = ArithInOut.rrEffortJoinMeet sample eff
    sample = ipolycfg_sample_cf cfg

evalPolyDirect ::
    (Ord var, Show var, Show cf, Show val) => 
    (PolyEvalOps var cf val) ->
    [val] ->
    IntPoly var cf -> val
evalPolyDirect opsV values p@(IntPoly cfg terms)
    = 
--    unsafePrint
--    (
--        "evalPolyDirect: "
--        ++ "\n  values = " ++ show values
--        ++ "\n  p = " ++ show p
--    ) $
    ev values terms
    where
    zV = polyEvalZero opsV
    addV = polyEvalAdd opsV 
    multV = polyEvalMult opsV
    powV = polyEvalPow opsV
    cfV = polyEvalCoeff opsV
    polyV = polyEvalMaybePoly opsV
    ev [] (IntPolyC cf) = cfV cf
    ev (varValue : restVars) (IntPolyV _ polys)
        | IntMap.null polys = zV
        | lowestExponent == 0 = 
            resultMaybeWithoutConstantTerm
        | otherwise = 
            (varValue `powV` lowestExponent) `multV` resultMaybeWithoutConstantTerm 
        where
        (lowestExponent, resultMaybeWithoutConstantTerm) 
            = IntMap.foldWithKey addTerm (highestExponent, zV) polys 
        (highestExponent, _) = IntMap.findMax polys 
        addTerm exponent poly (prevExponent, prevVal) = (exponent, newVal)
            where
            newVal = -- Horner scheme:
                polyValue 
                `addV`
                (prevVal `multV` (varValue `powV` (prevExponent - exponent)))
            polyValue =
                case polyV poly of
                    Just value -> value
                    Nothing -> ev restVars poly  
    ev varVals terms =
        error $ "evalPolyDirect: illegal case: varVals = " ++ show varVals ++ "; terms = " ++ show terms

-- TODO: make the following function generic for any function representation with nominal derivatives    
evalPolyMono ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
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
--        ) $ 
        fromEndPtsV (left, right)
    where
    direct = evalPolyDirect opsV values p
    zV = polyEvalZero opsV
    addV = polyEvalAdd opsV 
    multV = polyEvalMult opsV
    powV = polyEvalPow opsV
    cfV = polyEvalCoeff opsV
    polyV = polyEvalMaybePoly opsV
    leqV = polyEvalLeq opsV
    (noMonoOps, monoOpsV) = 
        case polyEvalMonoOps opsV of
            Nothing -> (True, error "evalPolyMono: internal error: monoOpsV used when not present")
            Just monoOpsV -> (False, monoOpsV)
    fromEndPtsV = polyEvalMonoFromEndpoints monoOpsV
    getEndPtsV = polyEvalMonoGetEndpoints monoOpsV
    isThin = polyEvalMonoIsThin monoOpsV
    eff= polyEvalMonoCfEffortIndicator monoOpsV
    left = evalPolyDirect opsV valuesL p
    right = evalPolyDirect opsV valuesR p
    (noMonotoneVar, valuesL, valuesR) =
        detectMono True [] [] $ reverse $ zip vars $ values -- undo reverse due to the accummulators
    vars = ipolycfg_vars cfg
    sampleDom = head values
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
            case (valIsThin, zV `leqV` deriv, deriv `leqV` zV) of
                (True, _, _) -> (undefined, True) -- when a variable has a thin domain, do not bother separating endpoints 
                (_, Just True, _) -> (True, False) 
                (_, _, Just True) -> (False, False)
                _ -> (undefined, True)
        deriv = evalPolyDirect opsV values $ diffPoly eff var p -- range of (d p)/(d var)    
        (valL, valR) = getEndPtsV val
        valIsThin = isThin val
