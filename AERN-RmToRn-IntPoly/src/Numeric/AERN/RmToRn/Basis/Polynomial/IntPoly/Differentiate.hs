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
--    (
--    )
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps

import Numeric.AERN.RmToRn.New

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps

diffPoly ::
    (Ord var, ArithInOut.RoundedReal cf) => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    var ->
    IntPoly var cf ->
    IntPoly var cf
diffPoly eff var poly =
    dp poly
    where
    effMult = ArithInOut.mxfldEffortMult sample (1::Int) $ ArithInOut.rrEffortIntMixedField sample eff
    sample = ipolycfg_sample_cf cfg
    cfg = intpoly_cfg poly
    dp (IntPolyG cfg x [c]) = IntPolyG cfg x [zero]
    dp (IntPolyG cfg x coeffs) 
        =
        IntPolyG cfg x $ coeffsMultiples
        where
        coeffsMultiples = 
            let (<*>|) = ArithInOut.mixedMultOutEff effMult in
            zipWith (<*>|) coeffs [degree,degree-1..1]
        degree = length coeffs - 1
    dp (IntPolyV cfg x [p]) 
        | var == x = 
            IntPolyV cfg x $ [newConstFn cfg undefined zero]
    dp (IntPolyV cfg x polys)
        | var == x = 
            IntPolyV cfg x $ polysMultiples
        where
        polysMultiples = 
            let (<*>|) = ArithInOut.mixedMultOutEff effMult in
            zipWith (<*>|) polys [degree,degree-1..1]
        degree = length polys - 1
    dp (IntPolyV cfg x polys)
        =
        polyNormalise $ 
        IntPolyV cfg x $ map dp polys
