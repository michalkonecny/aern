{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.Elementary
    Description :  support for various common elementary functions
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Support for various common elementary functions.
    
    This module is hidden and reexported via its parent NumericOrderRounding. 
-}

module Numeric.AERN.RealArithmetic.NumericOrderRounding.Elementary where

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.FieldOps

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.ShowInternals
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.Misc.Debug

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class RoundedExponentiation t where
    type ExpEffortIndicator t
    expDefaultEffort :: t -> ExpEffortIndicator t
    expUpEff :: (ExpEffortIndicator t) -> t -> t
    expDnEff :: (ExpEffortIndicator t) -> t -> t

-- | @e^a*e^(-a) = 1@
propExpOfNegRecip ::
    (NumOrd.PartialComparison t, NumOrd.RoundedLattice t,
     RoundedExponentiation t, RoundedMultiply t, Neg t, HasOne t,
     Show t,
     ShowInternals t,
     Show (ExpEffortIndicator t),
     EffortIndicator (ExpEffortIndicator t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     (ExpEffortIndicator t, MultEffortIndicator t)) -> 
    t -> Bool
propExpOfNegRecip _ initEffort e1 =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff initEffort
    where
    expr1Up (effExp, effMult) = one
    expr1Dn (effExp, effMult) = one
    expr2Up (effExp, effMult) =
        let (*^) = multUpEff effMult in
        let expE1 = expUpEff effExp e1 in
        let expNegE1 = expUpEff effExp (neg e1) in
        let prod = expE1 *^ expNegE1 in
--        unsafePrintReturn
--        (
--          "propExpOfNegRecip: expr2Up: e1 = " ++ show e1 
--          ++ "; expE1 = " ++ show expE1 
--          ++ "; expNegE1 = " ++ show expNegE1 
--          ++ "; prod = " ++ showUsingShowInternals prod
--          ++ "; result = " 
--        )$
        prod
    expr2Dn (effExp, effMult) =
        let (*.) = multDnEff effMult in
        (expDnEff effExp e1) *. (expDnEff effExp (neg e1))

-- | @e^(b+c) = e^b * e^c@
propExpOfAddToMult ::
    (NumOrd.PartialComparison t,
     RoundedExponentiation t, RoundedMultiply t,  RoundedAdd t,
     Show t,
     Show (ExpEffortIndicator t),
     EffortIndicator (ExpEffortIndicator t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (NumOrd.PartialCompareEffortIndicator t, 
     (ExpEffortIndicator t, MultEffortIndicator t, AddEffortIndicator t)) -> 
    t -> t -> Bool
propExpOfAddToMult _ initEffort e1 e2 =
    equalRoundingUpDn
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff initEffort
    where
    expr1Up (effExp, effMult, effAdd) =
        let (+^) = addUpEff effAdd in
        (expUpEff effExp (e1 +^ e2))
    expr1Dn (effExp, effMult, effAdd) =
        let (+.) = addDnEff effAdd in
        (expDnEff effExp (e1 +. e2))
    expr2Up (effExp, effMult, effAdd) =
        let (*^) = multUpEff effMult in
        (expUpEff effExp e1) *^ (expUpEff effExp e2)
    expr2Dn (effExp, effMult, effAdd) =
        let (*.) = multDnEff effMult in
        (expDnEff effExp e1) *. (expDnEff effExp e2)
    
testsUpDnExp (name, sample) =
    testGroup (name ++ " exp up/dn") $
        [
            testProperty "e^a * e^(-a) = 1" (propExpOfNegRecip sample)
        ,
            testProperty "e^(a + b) = e^a * e^b" (propExpOfAddToMult sample)
        ]
    