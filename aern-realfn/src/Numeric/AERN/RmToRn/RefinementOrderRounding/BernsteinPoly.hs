{-# LANGUAGE FlexibleContexts #-}
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

--import Numeric.AERN.Misc.IntegerArithmetic

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
bernsteinOut (effRingF, effRealD, effMultFD) x n p =
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
        (binomialOut) .<*>~
            ((x ~<^> p) ~<*>~ ((c1 ~<->~ x) ~<^> (n-p)))
    binomialOut 
        | 0 <= p && p <= n =
            productNdownP .</>. factorialP
        | otherwise = 
            error $ 
                "bernsteinOut called with illegal parameters:" 
                ++ "\n n = " ++ show n 
                ++ "\n p = " ++ show p 
        where
        factorialP = 
            foldl (.<*>|) (one sampleD) [2..p]
        productNdownP = 
            foldl (.<*>|) (one sampleD) [(n-p+1)..n]

    c1 = (one sampleF)
    sampleF = x
    sampleD = getSampleDomValue sampleF
--    sampleCf = getSampleDomValue sampleF

--    (~<+>~) = ArithInOut.addOutEff effAddF
    (~<->~) = ArithInOut.subtrOutEff effAddF
    (~<*>~) = ArithInOut.multOutEff effMultF
    (~<^>) = ArithInOut.powerToNonnegIntOutEff effPowF
    (.<*>~) = flip $ ArithInOut.mixedMultOutEff effMultFD

    (.</>.) = ArithInOut.divOutEff effDivD
    (.<*>|) = ArithInOut.mixedMultOutEff effMultIntD

    effAddF = ArithInOut.ringEffortAdd sampleF effRingF
    effMultF = ArithInOut.ringEffortMult sampleF effRingF
    effPowF = ArithInOut.ringEffortPow sampleF effRingF
    effDivD = 
        ArithInOut.fldEffortDiv sampleD $ 
            ArithInOut.rrEffortField sampleD effRealD
    effMultIntD = 
        ArithInOut.mxfldEffortMult sampleD (1::Int) $ 
            ArithInOut.rrEffortIntMixedField sampleD effRealD
    
    
    