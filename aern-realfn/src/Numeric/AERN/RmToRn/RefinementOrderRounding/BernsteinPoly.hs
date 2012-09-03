{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Numeric.AERN.Misc.Debug
_ = unsafePrint

bernsteinOut ::
    (HasProjections f, HasConstFns f, HasOne f,
     ArithInOut.RoundedRing f,
     ArithInOut.RoundedMixedMultiply f (Domain f),
     ArithInOut.RoundedReal (Domain f), 
     Show f, Show (Domain f)) 
     =>
    (ArithInOut.RingOpsEffortIndicator f,
     ArithInOut.RoundedRealEffortIndicator (Domain f),
     ArithInOut.MixedMultEffortIndicator f (Domain f)) ->
    f ->
    Int ->
    Int ->
    f
bernsteinOut (effRing, effDom, effMMult) x n p =
--    unsafePrint
--    (
--        "bernsteinOut:"
--        ++ "\n x = " ++ show x
--        ++ "\n n = " ++ show n
--        ++ "\n p = " ++ show p
--        ++ "\n (n p) = " ++ show binomialOut
--        ++ "\n result = " ++ show result
--    )
    result
    where
    result = 
        let ?addInOutEffort = effAdd in
        let ?multInOutEffort = effMult in
        let ?mixedMultInOutEffort = effMMult in
        let ?intPowerInOutEffort = effPow in
        (binomialOut) |<*>
            ((x<^>p) <*> ((c1 <-> x)<^>(n-p)))
    binomialOut 
        | 0 <= p && p <= n =
            let ?divInOutEffort = effDivD in
            productNdownP </> factorialP
        | otherwise = 
            error $ 
                "bernsteinOut called with illegal parameters:" 
                ++ "\n n = " ++ show n 
                ++ "\n p = " ++ show p 
        where
        factorialP = 
            let ?mixedMultInOutEffort = effMultIntD in
            foldl (<*>|) (one sampleD) [2..p]
        productNdownP = 
            let ?mixedMultInOutEffort = effMultIntD in
            foldl (<*>|) (one sampleD) [(n-p+1)..n]

    sampleF = x
    sampleD = getSampleDomValue sampleF
    c1 = (one sampleF)
--    sampleCf = getSampleDomValue sampleF
    effAdd = ArithInOut.ringEffortAdd sampleF effRing
    effMult = ArithInOut.ringEffortMult sampleF effRing
    effPow = ArithInOut.ringEffortPow sampleF effRing
    effDivD = 
        ArithInOut.fldEffortDiv sampleD $ 
            ArithInOut.rrEffortField sampleD effDom
    effMultIntD = 
        ArithInOut.mxfldEffortMult sampleD (1::Int) $ 
            ArithInOut.rrEffortIntMixedField sampleD effDom
    
    
    