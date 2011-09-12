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

diffPoly ::
    (Ord var, ArithInOut.RoundedReal cf) => 
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    var ->
    IntPoly var cf ->
    IntPoly var cf
diffPoly eff var (IntPoly cfg poly) =
    IntPoly cfg $ dp poly
    where
    effMult = ArithInOut.mxfldEffortMult sample (1::Int) $ ArithInOut.rrEffortIntMixedField sample eff
    sample = ipolycfg_sample_cf cfg
    dp (IntPolyG x [c]) = IntPolyG x [zero]
    dp (IntPolyG x coeffs) 
        =
        IntPolyG x $ coeffsMultiples
        where
        coeffsMultiples = 
            let (<*>|) = ArithInOut.mixedMultOutEff effMult in
            zipWith (<*>|) coeffs [degree,degree-1..1]
        degree = length coeffs - 1
    dp (IntPolyV x [p]) 
        | var == x = 
            IntPolyV x $ [intpoly_terms $ newConstFn cfg undefined zero]
    dp (IntPolyV x polys)
        | var == x = 
            IntPolyV x $ polysMultiples
        where
        polysMultiples = 
            let (<*>|) = ArithInOut.mixedMultOutEff effMult in
            map intpoly_terms $ zipWith (<*>|) intpolys [degree,degree-1..1]
        intpolys = map (IntPoly cfg) polys
        degree = length polys - 1
    dp (IntPolyV x polys)
        =
        termsNormalise cfg $ 
        IntPolyV x $ map dp polys
