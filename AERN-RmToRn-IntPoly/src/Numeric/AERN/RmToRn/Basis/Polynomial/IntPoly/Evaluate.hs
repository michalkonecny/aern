{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluate
    Description :  evaluation of interval polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Evaluation of interval polynomials.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Evaluate
--    (
--    )
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Differentiate

import Numeric.AERN.RmToRn.New

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort
import Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap

data PolyEvalOps var cf val =
    PolyEvalOps
    {
        polyEvalZero :: val,
        polyEvalAdd :: (val -> val -> val),
        polyEvalMult :: (val -> val -> val),
        polyEvalPow :: (val -> Int -> val), {-^ non-negative integer power -}
        polyEvalCoeff :: (cf -> val), {-^ coeff conversion -}
        polyEvalMaybePoly :: (IntPolyTerms var cf -> Maybe val), {-^ optional direct poly conversion -}
        polyEvalJoin :: (val -> val -> val),
        polyEvalLeq :: (val -> val -> Maybe Bool)
    }

coeffPolyEvalOps z =
    PolyEvalOps z (<+>) (<*>) (<^>) id (const Nothing) (</\>) (<=?)

evalPolyAtPoint ::
    (Ord var, Show var, ArithInOut.RoundedReal cf, Show cf) => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf {- zero coefficient -} ->
    [cf] {- values for each variable respectively -} -> 
    IntPoly var cf -> cf
evalPolyAtPoint eff z values p@(IntPoly cfg _)
    = 
    let ?multInOutEffort = effMult in
    let ?intPowerInOutEffort = effPwr in
    let ?addInOutEffort = effAdd in
    let ?joinmeetOutEffort = effJoin in
    let ?pCompareEffort = effComp in
    evalPolyDirect (coeffPolyEvalOps z) values p
    where
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    effComp = ArithInOut.rrEffortNumComp sample eff
    effJoin = ArithInOut.rrEffortJoinMeetOut sample eff
    sample = ipolycfg_sample_cf cfg

evalPolyOnInterval ::
    (Ord var, Show var, ArithInOut.RoundedReal cf, Show cf) => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf {- zero coefficient -} ->
    [(cf, cf)] {- values for each variable respectively -} -> 
    IntPoly var cf -> (cf, cf)
evalPolyOnInterval eff z valuesLR p@(IntPoly cfg _)
    = 
    let ?multInOutEffort = effMult in
    let ?intPowerInOutEffort = effPwr in
    let ?addInOutEffort = effAdd in
    let ?joinmeetOutEffort = effJoin in
    let ?pCompareEffort = effComp in
    evalPolyMono eff (coeffPolyEvalOps z) valuesALR p
    where
    valuesALR = 
        let ?pCompareEffort = effComp in
        zip values $ map checkEqual valuesLR
        where
        values = 
            let ?joinmeetOutEffort = effJoin in
            map (uncurry (</\>)) valuesLR
        checkEqual (l,r) 
            | (l ==? r) == Just True = Nothing
            | otherwise = Just (l,r)
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    effComp = ArithInOut.rrEffortNumComp sample eff
    effJoin = ArithInOut.rrEffortJoinMeetOut sample eff
    sample = ipolycfg_sample_cf cfg


substPolyMainVar ::
    (Ord var, Show var, ArithInOut.RoundedReal cf, Show cf) => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf {- zero coefficient -} ->
    (IntPoly var cf, IntPoly var cf) {- polynomial interval to substitute the main var with -} -> 
    IntPoly var cf -> IntPoly var cf
substPolyMainVar eff z (IntPoly _ termsL, IntPoly _ termsR) p@(IntPoly cfg terms) =
    IntPoly cfg $ uncurry joinTerms $ 
        evalPolyMono eff substPolyEvalOps [(joinTerms termsL termsR, Just (termsL, termsR))] p
    where
    substPolyEvalOps =
        let ?multInOutEffort = effMult in
        let ?addInOutEffort = effAdd in
        let ?joinmeetOutEffort = effJoin in
        let ?pCompareEffort = effComp in
        PolyEvalOps 
            (cf2Terms z) addTerms multTerms (powTerms vars) 
            cf2Terms terms2terms joinTerms leqTerms
    cf2Terms cf = mkConstTerms cf vars
    terms2terms (IntPolyV v ts) | v == mainVar = Nothing
    terms2terms terms = Just $ IntPolyV mainVar $ IntMap.singleton 0 terms
    vars@(mainVar : _) = ipolycfg_vars cfg
    doms = ipolycfg_doms cfg
    leqTerms terms1 terms2 =
        zero <=? diffRange 
        where
        diffRange = uncurry (</\>) $ evalPolyOnInterval eff z domsR (IntPoly cfgR diff)
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
    effJoin = ArithInOut.rrEffortJoinMeetOut sample eff
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
    (Ord var, Show var, Show cf, ArithInOut.RoundedReal cf, Show val) => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    (PolyEvalOps var cf val) ->
    [(val, Maybe (val, val))] {-^  overall value, and optional lower and upper value for each variable -} -> 
    IntPoly var cf -> (val, val)
evalPolyMono eff opsV valuesALR p@(IntPoly cfg _)
    | noMonotoneVar = (direct, direct)
    | otherwise =
--        unsafePrint
--        (
--            "evalPolyMono: "
--            ++ "\n p = " ++ show p
--            ++ "\n values = " ++ show values
--            ++ "\n valuesL = " ++ show valuesL
--            ++ "\n valuesR = " ++ show valuesR
--        ) $ 
        (left, right)
    where
    direct = evalPolyDirect opsV values p
    zV = polyEvalZero opsV
    addV = polyEvalAdd opsV 
    multV = polyEvalMult opsV
    powV = polyEvalPow opsV
    cfV = polyEvalCoeff opsV
    polyV = polyEvalMaybePoly opsV
    joinV = polyEvalJoin opsV
    leqV = polyEvalLeq opsV
    values = map fst valuesALR
    left = evalPolyDirect opsV valuesL p
    right = evalPolyDirect opsV valuesR p
    (noMonotoneVar, valuesL, valuesR) =
        detectMono True [] [] $ reverse $ zip vars $ valuesALR -- undo reverse due to the accummulators
    vars = ipolycfg_vars cfg
    detectMono noMonotoneVarPrev valuesLPrev valuesRPrev []
        = (noMonotoneVarPrev, valuesLPrev, valuesRPrev)
    detectMono noMonotoneVarPrev valuesLPrev valuesRPrev ((var, (val, maybeValLR)) : rest)
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
            case (maybeValLR, zV `leqV` deriv, deriv `leqV` zV) of
                (Nothing, _, _) -> (undefined, True) -- when a variable has a thin domain, do not bother separating endpoints 
                (_, Just True, _) -> (True, False) 
                (_, _, Just True) -> (False, False)
                _ -> (undefined, True)
        Just (valL, valR) = maybeValLR
        deriv = evalPolyDirect opsV values $ diffPoly eff var p -- range of (d p)/(d var)    
    
    
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