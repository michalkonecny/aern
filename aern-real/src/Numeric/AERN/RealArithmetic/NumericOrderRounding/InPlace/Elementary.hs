{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.InPlace.Elementary
    Description :  support for various common elementary functions
    Copyright   :  (c) Michal Konecny, Jan Duracz
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
import Numeric.AERN.Basics.Exception (HasLegalValues)
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.NumericOrder as NumOrd

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class (RoundedExponentiation t, CanBeMutable t) => RoundedExponentiationInPlace t where
    expUpInPlaceEff :: OpMutable1Eff (ExpEffortIndicator t) t s
    expDnInPlaceEff :: OpMutable1Eff (ExpEffortIndicator t) t s

-- | Upward rounded in-place exponential with default effort
expUpInPlace :: (RoundedExponentiationInPlace t) => OpMutable1 t s
expUpInPlace = mutable1EffToMutable1 expUpInPlaceEff expDefaultEffort 

-- | Downward rounded in-place exponential with default effort
expDnInPlace :: (RoundedExponentiationInPlace t) => OpMutable1 t s
expDnInPlace = mutable1EffToMutable1 expDnInPlaceEff expDefaultEffort 


expUpInPlaceEffFromPure,
 expDnInPlaceEffFromPure ::
    (CanBeMutable t, RoundedExponentiation t) =>
    OpMutable1Eff (ExpEffortIndicator t) t s
expUpInPlaceEffFromPure =
    pureToMutable1Eff expUpEff
expDnInPlaceEffFromPure =
    pureToMutable1Eff expDnEff

expUpInPlaceEffFromInPlace,
 expDnInPlaceEffFromInPlace ::
    (RoundedExponentiationInPlace t) =>
    (ExpEffortIndicator t) -> t -> t
expUpInPlaceEffFromInPlace = 
    mutable1EffToPure expUpInPlaceEff 
expDnInPlaceEffFromInPlace = 
    mutable1EffToPure expDnInPlaceEff 

propUpDnExpInPlace ::
    (NumOrd.PartialComparison t, 
     RoundedExponentiationInPlace t, 
     RoundedExponentiation t, 
     Neg t,
     Show t, HasLegalValues t) 
    =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     ExpEffortIndicator t) -> 
    (NumOrd.UniformlyOrderedSingleton t) -> 
    Bool
propUpDnExpInPlace sample initEffort (NumOrd.UniformlyOrderedSingleton e1) =
    equalRoundingUpDn "in-place rounded exp"
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff initEffort
    where
    expUpEffViaInPlace = mutable1EffToPure expUpInPlaceEff
    expDnEffViaInPlace = mutable1EffToPure expDnInPlaceEff
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
    sqrtUpInPlaceEff :: OpMutable1Eff (SqrtEffortIndicator t) t s
    sqrtDnInPlaceEff :: OpMutable1Eff (SqrtEffortIndicator t) t s

-- | Upward rounded in-place square root with default effort
sqrtUpInPlace :: (RoundedSquareRootInPlace t) => OpMutable1 t s
sqrtUpInPlace = mutable1EffToMutable1 sqrtUpInPlaceEff sqrtDefaultEffort 

-- | Downward rounded in-place square root with default effort
sqrtDnInPlace :: (RoundedSquareRootInPlace t) => OpMutable1 t s
sqrtDnInPlace = mutable1EffToMutable1 sqrtDnInPlaceEff sqrtDefaultEffort 

sqrtUpInPlaceEffFromPure,
 sqrtDnInPlaceEffFromPure ::
    (CanBeMutable t, RoundedSquareRoot t) =>
    OpMutable1Eff (SqrtEffortIndicator t) t s
sqrtUpInPlaceEffFromPure =
    pureToMutable1Eff sqrtUpEff
sqrtDnInPlaceEffFromPure =
    pureToMutable1Eff sqrtDnEff

sqrtUpInPlaceEffFromInPlace,
 sqrtDnInPlaceEffFromInPlace ::
    (RoundedSquareRootInPlace t) =>
    (SqrtEffortIndicator t) -> t -> t 
sqrtUpInPlaceEffFromInPlace = 
    mutable1EffToPure sqrtUpInPlaceEff 
sqrtDnInPlaceEffFromInPlace = 
    mutable1EffToPure sqrtDnInPlaceEff 

propUpDnSqrtInPlace ::
    (NumOrd.PartialComparison t, 
     RoundedSquareRootInPlace t, 
     RoundedSquareRoot t, 
     Neg t,
     Show t, HasLegalValues t) 
    =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     SqrtEffortIndicator t) -> 
    (NumOrd.UniformlyOrderedSingleton t) -> 
    Bool
propUpDnSqrtInPlace sample initEffort (NumOrd.UniformlyOrderedSingleton e1) =
    equalRoundingUpDn "in-place rounded sqrt"
        sqrtr1Up sqrtr1Dn sqrtr2Up sqrtr2Dn 
        NumOrd.pLeqEff initEffort
    where
    sqrtUpEffViaInPlace = mutable1EffToPure sqrtUpInPlaceEff
    sqrtDnEffViaInPlace = mutable1EffToPure sqrtDnInPlaceEff
    sqrtr1Up eff = sqrtUpEff eff e1
    sqrtr1Dn eff = sqrtDnEff eff e1
    sqrtr2Up eff = sqrtUpEffViaInPlace eff e1
    sqrtr2Dn eff = sqrtDnEffViaInPlace eff e1

testsUpDnSqrtInPlace (name, sample) =
    testGroup (name ++ " in place sqrt") $
        [
            testProperty "matches pure" (propUpDnSqrtInPlace sample)
        ]
        