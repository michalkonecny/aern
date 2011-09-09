{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
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

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps

diffPoly ::
    (Eq var, HasZero cf, ArithInOut.RoundedMixedMultiply cf Int) => 
    (ArithInOut.MixedMultEffortIndicator cf Int) ->
    var ->
    IntPoly var cf ->
    IntPoly var cf 
diffPoly effMult _ (IntPolyG cfg x [c]) = IntPolyG cfg x [zero]
diffPoly effMult _ (IntPolyG cfg x coeffs) =
    IntPolyG cfg x $ coeffsMultiples
    where
    coeffsMultiples = 
        let ?mixedMultInOutEffort = effMult in
        zipWith (<*>|) coeffs [degree,degree-1..1]
    degree = length coeffs - 1
--diffPoly var (IntPolyV cfg x [p]) 
--    | var == x = IntPolyV cfg x $ [newConstFn p zero]
--diffPoly var (V x polys) =
--    V x $ polysMultiples
--    where
--    polysMultiples = zipWith scalePolyByInt [degree,degree-1..1] polys
--    degree = length polys - 1
--diffPoly zero n (V x polys@(p:_)) =
--    polyNormalise $
--        V x $ map (diffPoly zero (n-1)) polys
