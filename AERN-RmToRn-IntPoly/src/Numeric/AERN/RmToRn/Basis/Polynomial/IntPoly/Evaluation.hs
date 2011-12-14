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
        where
        vals = map getValue vars
        vars = ipolycfg_vars cfg
        getValue var = case lookupVar valsMap var of Just val -> val

instance
    (Ord var, Show var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, Show cf)
    =>
    (HasEvalOps (IntPoly var cf)) cf
    where
    type EvalOpsEffortIndicator (IntPoly var cf) cf = ArithInOut.RoundedRealEffortIndicator cf
    evalOpsDefaultEffort _ sampleCf = ArithInOut.roundedRealDefaultEffort sampleCf 
    evalOpsOut eff sampleP sampleCf = 
        let ?multInOutEffort = effMult in
        let ?intPowerInOutEffort = effPwr in
        let ?addInOutEffort = effAdd in
        let ?joinmeetEffort = effJoin in
        let ?pCompareEffort = effComp in
        coeffPolyEvalOps sampleCf
        where
        effMult = ArithInOut.fldEffortMult sampleCf $ ArithInOut.rrEffortField sampleCf eff
        effPwr = ArithInOut.fldEffortPow sampleCf $ ArithInOut.rrEffortField sampleCf eff
        effAdd = ArithInOut.fldEffortAdd sampleCf $ ArithInOut.rrEffortField sampleCf eff
        effComp = ArithInOut.rrEffortNumComp sampleCf eff
        effJoin = ArithInOut.rrEffortJoinMeet sampleCf eff

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
        polyEvalMonoOps :: Maybe (PolyEvalMonoOps val)
    }

data PolyEvalMonoOps val =
    PolyEvalMonoOps
    {
        polyEvalMonoGetEndpoints :: val -> (val, val),
        polyEvalMonoFromEndpoints :: (val, val) -> val,
        polyEvalMonoIsThin :: val -> Bool
    }

coeffPolyEvalOps z =
    PolyEvalOps z (<+>) (<*>) (<^>) id (const Nothing) (<=?) $
        Just $ PolyEvalMonoOps
            RefOrd.getEndpointsOutWithDefaultEffort
            RefOrd.fromEndpointsOutWithDefaultEffort
            isDefinitelyExact
    where
    isDefinitelyExact a =
        (isExactEff $ imprecisionDefaultEffort a) a == Just True

evalPolyAtPoint ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, Show cf) 
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf {- zero coefficient -} ->
    [cf] {- values for each variable respectively -} -> 
    IntPoly var cf -> cf
evalPolyAtPoint eff z values p@(IntPoly cfg _)
    = 
    let ?multInOutEffort = effMult in
    let ?intPowerInOutEffort = effPwr in
    let ?addInOutEffort = effAdd in
    let ?joinmeetEffort = effJoin in
    let ?pCompareEffort = effComp in
    evalPolyDirect (coeffPolyEvalOps z) values p
    where
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    effComp = ArithInOut.rrEffortNumComp sample eff
    effJoin = ArithInOut.rrEffortJoinMeet sample eff
    sample = ipolycfg_sample_cf cfg

evalPolyOnInterval ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, 
     Show cf) 
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf {- zero coefficient -} ->
    [cf] {- values for each variable respectively -} -> 
    IntPoly var cf -> cf
evalPolyOnInterval eff z values p@(IntPoly cfg _)
    = 
    let ?multInOutEffort = effMult in
    let ?intPowerInOutEffort = effPwr in
    let ?addInOutEffort = effAdd in
    let ?joinmeetEffort = effJoin in
    let ?pCompareEffort = effComp in
    evalPolyMono eff (coeffPolyEvalOps z)  values p
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
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    effComp = ArithInOut.rrEffortNumComp sample eff
    effJoin = ArithInOut.rrEffortJoinMeet sample eff
    effImpr = ArithInOut.rrEffortImprecision sample eff
    sample = ipolycfg_sample_cf cfg

substPolyMainVarElim ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf, 
     Show cf) 
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf {- zero coefficient -} ->
    cf {- value to substitute the main var with -} ->
    IntPoly var cf -> IntPoly var cf
substPolyMainVarElim eff z substVal p@(IntPoly cfg _)
    = 
    case substPolyMainVar eff z substPoly p of
        IntPoly cfg (IntPolyV _ coeffs) ->
            IntPoly cfgR terms
            where
            cfgR = cfgRemVar cfg
            terms = case IntMap.toList coeffs of [(0, terms)] -> terms
    where
    substPoly = mkPoly substVal
    mkPoly = newConstFn cfg undefined

substPolyMainVar ::
    (Ord var, Show var, 
     ArithInOut.RoundedReal cf, Show cf,
     RefOrd.IntervalLike cf) 
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf {- zero coefficient -} ->
    IntPoly var cf {- polynomial to substitute the main var with -} ->
    IntPoly var cf -> IntPoly var cf
substPolyMainVar eff z substPoly p@(IntPoly cfg terms) =
--    unsafePrint
--    (
--        "substPolyMainVar: "
--        ++ "\n p = " ++ showP p
--        ++ "\n substPoly = " ++ showP substPoly
--        ++ "\n result = " ++ showP result
--    ) $
    result
    where
--    showP = showPoly show show
    result =
        IntPoly cfg $
            evalPolyMono eff substPolyEvalOps [substTerms] p
    isDefinitelyExact p = termsAreExactEff effImpr p == Just True
    termsFromEndpoints (terms1, terms2) =
        intpoly_terms $ 
            RefOrd.fromEndpointsOutWithDefaultEffort (IntPoly cfg terms1, IntPoly cfg terms2)
    termsGetEndpoints terms =
        (intpoly_terms poly1, intpoly_terms poly2)
        where
        (poly1, poly2) = 
            RefOrd.getEndpointsOutWithDefaultEffort (IntPoly cfg terms)
    substTerms = intpoly_terms substPoly
    substPolyEvalOps =
        let ?multInOutEffort = effMult in
        let ?addInOutEffort = effAdd in
        let ?joinmeetEffort = effJoin in
        let ?pCompareEffort = effComp in
        PolyEvalOps 
            (cf2Terms z) addTerms multTerms (powTerms sample vars) 
            cf2Terms terms2terms leqTerms $
            Just $ PolyEvalMonoOps 
                termsGetEndpoints termsFromEndpoints isDefinitelyExact
    cf2Terms cf = mkConstTerms cf vars
    terms2terms (IntPolyV v ts) | v == mainVar = Nothing
    terms2terms terms = Just $ IntPolyV mainVar $ IntMap.singleton 0 terms
    vars@(mainVar : _) = ipolycfg_vars cfg
    doms = ipolycfg_doms cfg
    leqTerms terms1 terms2 =
        (zero sample) <=? diffRange 
        where
        diffRange = evalPolyOnInterval eff z domsR (IntPoly cfgR diff)
        diff = addTerms terms2 $ negTerms terms1
        (cfgR, domsR) =
            case terms1 of
                IntPolyV v _ | v == mainVar -> (cfg, doms)
                _ -> (cfgR, domsR)
            where
            cfgR = cfgRemVar cfg
            domR = ipolycfg_doms cfgR
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    effComp = ArithInOut.rrEffortNumComp sample eff
    effJoin = ArithInOut.rrEffortJoinMeet sample eff
    effImpr = ArithInOut.rrEffortImprecision sample eff
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
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    (PolyEvalOps var cf val) ->
    [val] {-^ value for each variable -} -> 
    IntPoly var cf -> val
evalPolyMono eff opsV values p@(IntPoly cfg _)
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

    
--evalPolyMono ::
--    (Ord var, Show var, ArithInOut.RoundedReal cf, Show cf) => 
--    (ArithInOut.RoundedRealEffortIndicator cf) ->
--    cf {- zero coefficient -} ->
--    [(cf, cf)] -> IntPoly var cf -> (cf, cf)
--evalPolyMono eff z valuesLR p@(IntPoly cfg terms)
--    | noMonotoneVar = (direct, direct)
--    | otherwise =
----        unsafePrint
----        (
----            "evalPolyMono: "
----            ++ "\n p = " ++ show p
----            ++ "\n values = " ++ show values
----            ++ "\n valuesL = " ++ show valuesL
----            ++ "\n valuesR = " ++ show valuesR
----        ) $ 
--        (left, right)
--    where
--    direct = evalPolyAtPoint eff z values p
--    values = 
--        let ?joinmeetOutEffort = effJoin in
--        map (uncurry (</\>)) valuesLR
--    effComp = ArithInOut.rrEffortNumComp sample eff
--    effJoin = ArithInOut.rrEffortJoinMeetOut sample eff
--    sample = ipolycfg_sample_cf cfg
--    left = evalPolyAtPoint eff z valuesL p
--    right = evalPolyAtPoint eff z valuesR p
--    (noMonotoneVar, valuesL, valuesR) =
--        let ?joinmeetOutEffort = effJoin in
--        detectMono True [] [] $ reverse $ zip vars valuesLR -- undo reverse due to the accummulators
--    vars = ipolycfg_vars cfg
--    detectMono noMonotoneVarPrev valuesLPrev valuesRPrev [] 
--        = (noMonotoneVarPrev, valuesLPrev, valuesRPrev)
--    detectMono noMonotoneVarPrev valuesLPrev valuesRPrev ((var, (valL, valR)) : rest)
--        =
----        unsafePrint 
----        (
----            "evalPolyMono: detectMono: deriv = " ++ show deriv
----        ) $ 
--        detectMono noMonotoneVarNew (valLNew : valuesLPrev) (valRNew : valuesRPrev) rest
--        where
--        noMonotoneVarNew = noMonotoneVarPrev && varNotMono
--        (valLNew, valRNew)
--            | varNotMono = (val, val) -- not monotone, we have to be safe
--            | varNonDecr = (valL, valR) -- non-decreasing on the whole domain - can use endpoints
--            | otherwise = (valR, valL) -- non-increasing on the whole domain - can use swapped endpoints
--        val = valL </\> valR
--        (varNonDecr, varNotMono) =
--            let _ = [deriv, valL] in -- unify types of these vars so that the following effort can apply to them all  
--            let ?pCompareEffort = effComp in
--            case (valL ==? valR, zero <=? deriv, deriv <=? zero) of
--                (Just True, _, _) -> (undefined, True) -- when a variable has a thin domain, do not bother separating endpoints 
--                (_, Just True, _) -> (True, False) 
--                (_, _, Just True) -> (False, False)
--                _ -> (undefined, True)  
--        deriv = evalPolyAtPoint eff z values $ diffPoly eff var p -- range of (d p)/(d var)    
--    
--    
--                