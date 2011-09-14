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

evalPolyDirect ::
    (Ord var, Show var, ArithInOut.RoundedReal cf, Show cf) => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf {- zero coefficient -} ->
    [(cf, cf)] -> IntPoly var cf -> cf
evalPolyDirect eff z values p@(IntPoly cfg terms)
    = 
--    unsafePrint
--    (
--        "evalPolyDirect: "
--        ++ "\n  values = " ++ show values
--        ++ "\n  p = " ++ show p
--    ) $
    let ?multInOutEffort = effMult in
    let ?addInOutEffort = effAdd in
    let ?joinmeetOutEffort = effJoin in
    ev values terms
    where
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    effJoin = ArithInOut.rrEffortJoinMeetOut sample eff
    sample = ipolycfg_sample_cf cfg
    ev [(valL, valR)] (IntPolyG _ coeffs@(coeffHighestPower : rest)) 
        =
        aux rest coeffHighestPower
        where
        val = valL </\> valR
        aux [] acc = acc
        aux (coeffHighestPower : rest) acc =
            aux rest newAcc
            where
            newAcc = coeffHighestPower <+> (val <*> acc)
    ev ((valL, valR) : restVars) (IntPolyV _ polys@(polyHighestPower : restPolys))
        =
        aux restPolys (ev restVars polyHighestPower)
        where
        val = valL </\> valR
        aux [] acc = acc
        aux (polyHighestPower : restPolys) acc =
            aux restPolys newAcc
            where
            newAcc = (ev restVars polyHighestPower) <+> (val <*> acc)
    ev varVals terms =
        error $ "evalPolyDirect: illegal case: varVals = " ++ show varVals ++ "; terms = " ++ show terms
--        foldl (<+>) z $ zipWith (<*>) xPowers $ map (ev rest) polys
--        where
--        x = valL </\> valR
--        xPowers = mkPowers (z <+> one) [] polys
--        mkPowers xN acc [] = acc
--        mkPowers xN acc (_:rest) = mkPowers (xN <*> x) (xN : acc) rest
    
evalPolyMono ::
    (Ord var, Show var, ArithInOut.RoundedReal cf, Show cf) => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf {- zero coefficient -} ->
    [(cf, cf)] -> IntPoly var cf -> (cf, cf)
evalPolyMono eff z values p@(IntPoly cfg terms)
    | noMonotoneVar = (direct, direct)
    | otherwise = (left, right)
    where
    direct = evalPolyDirect eff z values p
    effComp = ArithInOut.rrEffortNumComp sample eff
    effJoin = ArithInOut.rrEffortJoinMeetOut sample eff
    sample = ipolycfg_sample_cf cfg
    left = evalPolyDirect eff z valuesL p
    right = evalPolyDirect eff z valuesR p
    (noMonotoneVar, valuesL, valuesR)
        = detectMono True [] [] $ reverse $ zip vars values -- undo reverse due to the accummulators
    vars = ipolycfg_vars cfg
    detectMono noMonotoneVarPrev valuesLPrev valuesRPrev [] 
        = (noMonotoneVarPrev, valuesLPrev, valuesRPrev)
    detectMono noMonotoneVarPrev valuesLPrev valuesRPrev ((var, val@(valL, valR)) : rest)
        = detectMono noMonotoneVarNew (valLNew : valuesLPrev) (valRNew : valuesRPrev) rest
        where
        noMonotoneVarNew = noMonotoneVarPrev && varNotMono
        (valLNew, valRNew)
            | varNotMono = (val, val) -- not monotone, we have to be safe
            | varNonDecr = ((valL, valL), (valR, valR)) -- non-decreasing on the whole domain - can use endpoints
            | otherwise = ((valR, valR), (valL, valL)) -- non-increasing on the whole domain - can use swapped endpoints
        (varNonDecr, varNotMono) =
            let _ = [deriv, valL] in -- unify types of these vars so that the following effort can apply to them all  
            let ?pCompareEffort = effComp in
            case (valL ==? valR, zero <=? deriv, deriv <=? zero) of
                (Just True, _, _) -> (undefined, True) -- when a variable has a thin domain, do not bother separating endpoints 
                (_, Just True, _) -> (True, False) 
                (_, _, Just True) -> (False, False)
                _ -> (undefined, True)  
        deriv = evalPolyDirect eff z values $ diffPoly eff var p -- range of (d p)/(d var)    
    
    
        