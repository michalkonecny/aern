{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.Elementary
    Description :  support for various common elementary functions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Support for various common elementary functions.
    
    This module is hidden and reexported via its parent RefinementOrderRounding.InPlace. 
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.Elementary where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Elementary

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class (RoundedExponentiation t, CanBeMutable t) => RoundedExponentiationInPlace t where
    expInInPlaceEff :: t -> OpMutable1Eff (ExpEffortIndicator t) t s
    expOutInPlaceEff :: t -> OpMutable1Eff (ExpEffortIndicator t) t s
    expInInPlaceEff sample =
        pureToMutable1Eff sample expInEff
    expOutInPlaceEff sample =
        pureToMutable1Eff sample expOutEff

propInOutExpInPlace ::
    (RefOrd.PartialComparison t, RoundedExponentiationInPlace t, Neg t,
     Show t,
     Show (ExpEffortIndicator t),
     EffortIndicator (ExpEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     ExpEffortIndicator t) -> 
    t -> Bool
propInOutExpInPlace sample initEffort e1 =
    equalRoundingUpDn
        expr1In expr1Out expr2In expr2Out 
        RefOrd.pLeqEff initEffort
    where
    expInEffViaInPlace = mutable1EffToPure (expInInPlaceEff sample)
    expOutEffViaInPlace = mutable1EffToPure (expOutInPlaceEff sample)
    expr1In eff = expInEff eff e1
    expr1Out eff = expOutEff eff e1
    expr2In eff = expInEffViaInPlace eff e1
    expr2Out eff = expOutEffViaInPlace eff e1

testsInOutExpInPlace (name, sample) =
    testGroup (name ++ " in place exp") $
        [
            testProperty "matches pure" (propInOutExpInPlace sample)
        ]
