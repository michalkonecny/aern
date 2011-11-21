{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Differentiate
    Description :  symbolic differentiation of interval polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Symbolic differentiation of interval polynomials.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Differentiate
    (
        diffPoly
    )
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps

import Numeric.AERN.RmToRn.New

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Data.IntMap as IntMap

diffPoly ::
    (Ord var, ArithInOut.RoundedReal cf) => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    var ->
    IntPoly var cf ->
    IntPoly var cf
diffPoly eff var (IntPoly cfg poly) =
    IntPoly cfg $ dp cfg poly
    where
    effMult = ArithInOut.mxfldEffortMult sampleCf (1::Int) $ ArithInOut.rrEffortIntMixedField sampleCf eff
    sampleCf = ipolycfg_sample_cf cfg
    dp cfg (IntPolyC val) = IntPolyC $ zero sampleCf
    dp cfg (IntPolyV x polys)
        | var == x = IntPolyV x $ polysMultiples
        where
        polysMultiples = 
            IntMap.fromDistinctAscList $
            map diffTerm $
            IntMap.toAscList $ IntMap.delete 0 polys
        diffTerm (n,p) =
            let (<*>|) = ArithInOut.mixedMultOutEff effMult in
            (n - 1, intpoly_terms $ (IntPoly cfgR p) <*>| n)
        cfgR = cfgRemVar cfg 
    dp cfg (IntPolyV x polys)
        =
        termsNormalise cfgR $ 
        IntPolyV x $ IntMap.map (dp cfgR) polys
        where
        cfgR = cfgRemVar cfg 
        