{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Differentiation
    Description :  symbolic differentiation of interval polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Symbolic differentiation of interval polynomials.
-}

module Numeric.AERN.Poly.IntPoly.Differentiation
    (
        diffPolyOut
    )
where
    
import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.New ()

import Numeric.AERN.RmToRn.Differentiation

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps

import qualified Data.IntMap as IntMap

instance
    (Ord var, ArithInOut.RoundedReal cf)
    => 
    RoundedFakeDerivative (IntPoly var cf)
    where
    type FakeDerivativeEffortIndicator (IntPoly var cf) = ArithInOut.RoundedRealEffortIndicator cf
    fakeDerivativeDefaultEffort (IntPoly cfg _) = ArithInOut.roundedRealDefaultEffort sampleCf
        where
        sampleCf = ipolycfg_sample_cf cfg
    fakePartialDerivativeOutEff effCf var p = diffPolyOut effCf p var
    fakePartialDerivativeInEff = 
        error "inner rounded fake derivative of IntPoly not implemented yet"

{-|
    Symbolic differentiation of a polynomial by one of its variables.
    
    /Beware: The result does not enclose the derivative of any function
    approximated by the parameter polynomial.  This function can be used
    only when the polynomial is seen as a itself, not as a function
    approximant./
-}
diffPolyOut ::
    (Ord var, ArithInOut.RoundedReal cf) 
    => 
    (ArithInOut.RoundedRealEffortIndicator cf) {-^ effort indicator for coefficient operations -} ->
    var {-^ variable to differentiate by -} ->
    IntPoly var cf ->
    IntPoly var cf
diffPolyOut effCf var (IntPoly cfg poly) =
--    let ?mixedMultInOutEffort = effMult in
    IntPoly cfg $ dp cfg poly
    where
    effMult = ArithInOut.mxfldEffortMult sampleCf (1::Int) $ ArithInOut.rrEffortIntMixedField sampleCf effCf
    sampleCf = ipolycfg_sample_cf cfg
    dp _cfg (IntPolyC _val) = IntPolyC $ zero sampleCf
    dp _cfg (IntPolyV x polys)
        | var == x = IntPolyV x $ polysMultiples
        where
        polysMultiples = 
            IntMap.fromDistinctAscList $
            map diffTerms $
                IntMap.toAscList $ IntMap.delete 0 polys
        diffTerms (n,terms) =
--            let (<*>|) = ArithInOut.mixedMultOutEff effMult in
            let ?mixedMultInOutEffort = effMult in
            (n - 1, termsMapCoeffs (<*>| n) terms)
    dp cfg2 (IntPolyV x polys)
        =
        termsNormalise $ 
            IntPolyV x $ IntMap.map (dp cfgR) polys
        where
        cfgR = cfgRemFirstVar cfg2 
        