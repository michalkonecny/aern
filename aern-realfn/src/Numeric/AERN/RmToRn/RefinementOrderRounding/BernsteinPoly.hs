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
    (HasProjections f, HasConstFns f, HasOne f,
     ArithInOut.RoundedRing f,
     ArithInOut.RoundedMixedField f Int) 
     =>
    (ArithInOut.RingOpsEffortIndicator f,
     ArithInOut.MixedFieldOpsEffortIndicator f Int) ->
    f ->
    Int ->
    Int ->
    f
bernsteinOut (effRing, effIntOps) x n p =
    let ?addInOutEffort = effAdd in
    let ?multInOutEffort = effMult in
    let ?mixedMultInOutEffort = effMMult in
    let ?intPowerInOutEffort = effPow in
    (binomial n p) |<*>
        ((x<^>p) <*> ((c1 <-> x)<^>(n-p)))
    where
    sampleF = x
    c1 = (one sampleF)
    sampleCf = getSampleDomValue sampleF
    effAdd = ArithInOut.ringEffortAdd sampleF effRing
    effMult = ArithInOut.ringEffortMult sampleF effRing
    effPow = ArithInOut.ringEffortPow sampleF effRing
    effMMult = ArithInOut.mxfldEffortMult sampleF (1::Int) effIntOps
    