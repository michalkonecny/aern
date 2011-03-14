{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.Elementary
    Description :  support for various common elementary functions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Support for various common elementary functions.
    
    This module is hidden and reexported via its parent NumericOrderRounding.InPlace. 
-}

module Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.Elementary where

import Numeric.AERN.RealArithmetic.NumericOrderRounding.Elementary

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.FieldOps

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class (RoundedExponentiation t, CanBeMutable t) => RoundedExponentiationInPlace t where
    expUpInPlaceEff :: t -> OpMutable1Eff (ExpEffortIndicator t) t s
    expDnInPlaceEff :: t -> OpMutable1Eff (ExpEffortIndicator t) t s

expUpInPlaceEffFromPure sample =
    pureToMutable1Eff sample expUpEff
expDnInPlaceEffFromPure sample =
    pureToMutable1Eff sample expDnEff

expUpInPlaceEffFromInPlace sample = 
    mutable1EffToPure $ expUpInPlaceEff sample 
expDnInPlaceEffFromInPlace sample = 
    mutable1EffToPure $ expDnInPlaceEff sample 

propUpDnExpInPlace ::
    (NumOrd.PartialComparison t, 
     RoundedExponentiationInPlace t, 
     RoundedExponentiation t, 
     Neg t,
     Show t,
     Show (ExpEffortIndicator t),
     EffortIndicator (ExpEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     ExpEffortIndicator t) -> 
    t -> Bool
propUpDnExpInPlace sample initEffort e1 =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff initEffort
    where
    expUpEffViaInPlace = mutable1EffToPure (expUpInPlaceEff sample)
    expDnEffViaInPlace = mutable1EffToPure (expDnInPlaceEff sample)
    expr1Up eff = expUpEff eff e1
    expr1Dn eff = expDnEff eff e1
    expr2Up eff = expUpEffViaInPlace eff e1
    expr2Dn eff = expDnEffViaInPlace eff e1

testsUpDnExpInPlace (name, sample) =
    testGroup (name ++ " in place exp") $
        [
            testProperty "matches pure" (propUpDnExpInPlace sample)
        ]

        
class (RoundedSquareRoot t, CanBeMutable t) => RoundedSquareRootInPlace t where
    sqrtUpInPlaceEff :: t -> OpMutable1Eff (SqrtEffortIndicator t) t s
    sqrtDnInPlaceEff :: t -> OpMutable1Eff (SqrtEffortIndicator t) t s

sqrtUpInPlaceEffFromPure sample =
    pureToMutable1Eff sample sqrtUpEff
sqrtDnInPlaceEffFromPure sample =
    pureToMutable1Eff sample sqrtDnEff

sqrtUpInPlaceEffFromInPlace sample = 
    mutable1EffToPure $ sqrtUpInPlaceEff sample 
sqrtDnInPlaceEffFromInPlace sample = 
    mutable1EffToPure $ sqrtDnInPlaceEff sample 

propUpDnSqrtInPlace ::
    (NumOrd.PartialComparison t, 
     RoundedSquareRootInPlace t, 
     RoundedSquareRoot t, 
     Neg t,
     Show t,
     Show (SqrtEffortIndicator t),
     EffortIndicator (SqrtEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     SqrtEffortIndicator t) -> 
    t -> Bool
propUpDnSqrtInPlace sample initEffort e1 =
    equalRoundingUpDn
        sqrtr1Up sqrtr1Dn sqrtr2Up sqrtr2Dn 
        NumOrd.pLeqEff initEffort
    where
    sqrtUpEffViaInPlace = mutable1EffToPure (sqrtUpInPlaceEff sample)
    sqrtDnEffViaInPlace = mutable1EffToPure (sqrtDnInPlaceEff sample)
    sqrtr1Up eff = sqrtUpEff eff e1
    sqrtr1Dn eff = sqrtDnEff eff e1
    sqrtr2Up eff = sqrtUpEffViaInPlace eff e1
    sqrtr2Dn eff = sqrtDnEffViaInPlace eff e1

testsUpDnSqrtInPlace (name, sample) =
    testGroup (name ++ " in place sqrt") $
        [
            testProperty "matches pure" (propUpDnSqrtInPlace sample)
        ]
        