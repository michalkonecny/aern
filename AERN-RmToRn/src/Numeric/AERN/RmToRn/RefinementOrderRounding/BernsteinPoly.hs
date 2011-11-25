{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RmToRn.RefinementOrderRounding.BernsteinPoly
    Description :  approximations of Bernstein polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Approximations of Bernstein polynomials.
-}

module Numeric.AERN.RmToRn.RefinementOrderRounding.BernsteinPoly where

import Numeric.AERN.RmToRn.Domain
import Numeric.AERN.RmToRn.New

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import Numeric.AERN.Misc.IntegerArithmetic

bernsteinOut ::
    (HasProjections f, HasConstFns f,
     HasOne (Domain f),
     ArithInOut.RoundedAdd f, 
     ArithInOut.RoundedSubtr f, 
     ArithInOut.RoundedMultiply f,
     ArithInOut.RoundedMixedMultiply f Int,
     ArithInOut.RoundedPowerToNonnegInt f) 
     =>
    (ArithInOut.AddEffortIndicator f,
     ArithInOut.MultEffortIndicator f,
     ArithInOut.MixedMultEffortIndicator f Int,
     ArithInOut.PowerToNonnegIntEffortIndicator f) ->
    f ->
    (Var f) ->
    Int ->
    Int ->
    f
bernsteinOut eff@(effAdd, effMult, effMMult, effPwr) sampleF var n p =
    let ?addInOutEffort = effAdd in
    let ?multInOutEffort = effMult in
    let ?mixedMultInOutEffort = effMMult in
    let ?intPowerInOutEffort = effPwr in
    (binomial n p) |<*>
        ((x<^>p) <*> ((c1 <-> x)<^>(n-p)))
    where
    x = newProjectionFromSample sampleF var
    c1 = newConstFnFromSample sampleF (one sampleCf)
    sampleCf = getSampleDomValue sampleF
