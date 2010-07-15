{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing 2 types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations mixing 2 types.
    
    This module is hidden and reexported via its parent NumericOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.NumericOrderRounding.MixedFieldOps where

import Numeric.AERN.RealArithmetic.NumericOrderRounding.FieldOps
import Numeric.AERN.RealArithmetic.NumericOrderRounding.Conversion
import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class RoundedMixedAdd s t where
    type MixedAddEffortIndicator s t
    mixedAddDefaultEffort :: s -> t -> MixedAddEffortIndicator s t
    mixedAddUpEff :: MixedAddEffortIndicator s t -> s -> t -> t
    mixedAddDnEff :: MixedAddEffortIndicator s t -> s -> t -> t

{- tools to easily make a RoundedMixedAdd instance 
   via the composition of conversion and homogeneous addition -}

type MixedAddEffortIndicatorByConversion s t =
        (AddEffortIndicator t, ConvertEffortIndicator s t)

mixedAddDefaultEffortByConversion n d = 
        (addDefaultEffort d, convertDefaultEffort n d)

mixedAddUpEffByConversion ::
    (Convertible t1 t2, RoundedAdd t2) =>
    (AddEffortIndicator t2, ConvertEffortIndicator t1 t2) ->
    t1 -> t2 -> t2
mixedAddUpEffByConversion (effAdd, effConv) a b = 
    addUpEff effAdd (convertUpEff effConv a) b

mixedAddDnEffByConversion ::
    (Convertible t1 t2, RoundedAdd t2) =>
    (AddEffortIndicator t2, ConvertEffortIndicator t1 t2) ->
    t1 -> t2 -> t2
mixedAddDnEffByConversion (effAdd, effConv) a b = 
    addDnEff effAdd (convertDnEff effConv a) b

{- properties of mixed addition -}

propMixedAddEqualsConvert ::
    (NumOrd.PartialComparison t2, Convertible t1 t2,
     RoundedMixedAdd t1 t2, RoundedAdd t2,
     HasDistance t2,  Show (Distance t2),  
     NumOrd.PartialComparison (Distance t2), 
     HasInfinities (Distance t2), HasZero (Distance t2),
     Show (MixedAddEffortIndicator t1 t2),
     EffortIndicator (MixedAddEffortIndicator t1 t2),
     Show (ConvertEffortIndicator t1 t2),
     EffortIndicator (ConvertEffortIndicator t1 t2),
     Show (AddEffortIndicator t2),
     EffortIndicator (AddEffortIndicator t2),
     Show (NumOrd.PartialCompareEffortIndicator t2),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t2)
     ) =>
    t1 -> t2 ->
    (DistanceEffortIndicator t2) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t2)) -> 
    (NumOrd.PartialCompareEffortIndicator t2,
     (MixedAddEffortIndicator t1 t2,      
      AddEffortIndicator t2,
      ConvertEffortIndicator t1 t2)) -> 
    t1 -> t2 -> Bool
propMixedAddEqualsConvert sample1 sample2 effortDist effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1Up (effMAdd,_,_) =
        let (|+^) = mixedAddUpEff effMAdd in e1 |+^ e2
    expr1Dn (effMAdd,_,_) =
        let (|+.) = mixedAddDnEff effMAdd in e1 |+. e2
    expr2Up (_,effAdd,effConv) =
        let (+^) = addUpEff effAdd in (convertUpEff effConv e1) +^ e2
    expr2Dn (_,effAdd,effConv) =
        let (+.) = addDnEff effAdd in (convertDnEff effConv e1) +. e2

class RoundedMixedMultiply s t where
    type MixedMultEffortIndicator s t
    mixedMultDefaultEffort :: s -> t -> MixedMultEffortIndicator s t
    mixedMultUpEff :: MixedMultEffortIndicator s t -> s -> t -> t
    mixedMultDnEff :: MixedMultEffortIndicator s t -> s -> t -> t

{- tools to easily make a RoundedMixedMultiply instance 
   via the composition of conversion and homogeneous addition -}

type MixedMultEffortIndicatorByConversion s t =
        (MultEffortIndicator t, 
         ConvertEffortIndicator s t,
         NumOrd.MinmaxEffortIndicator t)

mixedMultDefaultEffortByConversion n d = 
        (addDefaultEffort d, 
         convertDefaultEffort n d,
         NumOrd.minmaxDefaultEffort d)

mixedMultUpEffByConversion ::
    (Convertible t1 t2, RoundedMultiply t2, NumOrd.RoundedLattice t2) =>
    (MultEffortIndicator t2, 
     ConvertEffortIndicator t1 t2,
     NumOrd.MinmaxEffortIndicator t2) ->
    t1 -> t2 -> t2
mixedMultUpEffByConversion (effMult, effConv, effMinmax) a b =
    NumOrd.maxUpEff effMinmax
    (multUpEff effMult (convertDnEff effConv a) b)
    (multUpEff effMult (convertUpEff effConv a) b)

mixedMultDnEffByConversion ::
    (Convertible t1 t2, RoundedMultiply t2, NumOrd.RoundedLattice t2) =>
    (MultEffortIndicator t2, 
     ConvertEffortIndicator t1 t2,
     NumOrd.MinmaxEffortIndicator t2) ->
    t1 -> t2 -> t2
mixedMultDnEffByConversion (effMult, effConv, effMinmax) a b = 
    NumOrd.maxDnEff effMinmax  
    (multDnEff effMult (convertDnEff effConv a) b)
    (multDnEff effMult (convertUpEff effConv a) b)

{- properties of mixed multiplication -}

propMixedMultEqualsConvert ::
    (NumOrd.PartialComparison t2, NumOrd.RoundedLattice t2, 
     Convertible t1 t2,
     RoundedMixedMultiply t1 t2, RoundedMultiply t2,
     HasDistance t2,  Show (Distance t2),  
     NumOrd.PartialComparison (Distance t2), 
     HasInfinities (Distance t2), HasZero (Distance t2),
     Show (MixedMultEffortIndicator t1 t2),
     EffortIndicator (MixedMultEffortIndicator t1 t2),
     Show (ConvertEffortIndicator t1 t2),
     EffortIndicator (ConvertEffortIndicator t1 t2),
     Show (MultEffortIndicator t2),
     EffortIndicator (MultEffortIndicator t2),
     Show (NumOrd.PartialCompareEffortIndicator t2),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t2),
     Show (NumOrd.MinmaxEffortIndicator t2),
     EffortIndicator (NumOrd.MinmaxEffortIndicator t2)
     ) =>
    t1 -> t2 ->
    (DistanceEffortIndicator t2) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t2)) -> 
    (NumOrd.PartialCompareEffortIndicator t2,
     (MixedMultEffortIndicator t1 t2,      
      (MultEffortIndicator t2,
       ConvertEffortIndicator t1 t2,
       NumOrd.MinmaxEffortIndicator t2))) -> 
    t1 -> t2 -> Bool
propMixedMultEqualsConvert sample1 sample2 effortDist effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1Up (effMMult,_) =
        let (|*^) = mixedMultUpEff effMMult in e1 |*^ e2
    expr1Dn (effMMult,_) =
        let (|*.) = mixedMultDnEff effMMult in e1 |*. e2
    expr2Up (_,(effMult,effConv,effMinmax)) =
        let (*^) = multUpEff effMult in
        NumOrd.maxUpEff effMinmax  
            ((convertUpEff effConv e1) *^ e2)
            ((convertDnEff effConv e1) *^ e2)
    expr2Dn (_,(effMult,effConv,effMinmax)) =
        let (*.) = multDnEff effMult in
        NumOrd.minDnEff effMinmax  
            ((convertUpEff effConv e1) *. e2)
            ((convertDnEff effConv e1) *. e2)


class RoundedMixedDivide s t where
    type MixedDivEffortIndicator s t
    mixedDivDefaultEffort :: s -> t -> MixedDivEffortIndicator s t
    mixedDivUpEff :: MixedDivEffortIndicator s t -> t -> s -> t
    mixedDivDnEff :: MixedDivEffortIndicator s t -> t -> s -> t

{- tools to easily make a RoundedMixedDivide instance 
   via the composition of conversion and homogeneous addition -}

type MixedDivEffortIndicatorByConversion s t =
        (DivEffortIndicator t, 
         ConvertEffortIndicator s t,
         (NumOrd.MinmaxEffortIndicator t,
          NumOrd.PartialCompareEffortIndicator t))

mixedDivDefaultEffortByConversion n d = 
        (addDefaultEffort d, 
         convertDefaultEffort n d,
         (NumOrd.minmaxDefaultEffort d,
          NumOrd.pCompareDefaultEffort d))

mixedDivUpEffByConversion ::
    (Convertible t1 t2, 
     RoundedDivide t2, 
     HasZero t2,  HasInfinities t2,
     NumOrd.PartialComparison t2,
     NumOrd.RoundedLattice t2) =>
    (DivEffortIndicator t2, 
     ConvertEffortIndicator t1 t2,
     (NumOrd.MinmaxEffortIndicator t2, 
      NumOrd.PartialCompareEffortIndicator t2)) ->
    t2 -> t1 -> t2
mixedDivUpEffByConversion (effDiv, effConv, (effMinmax, effComp)) a b =
    let ?pCompareEffort = effComp in
    case (bDn >=? zero, bUp <=? zero) of
        (Just True, _) -> normalResult 
        (_, Just True) -> normalResult
        _ -> plusInfinity -- b is too close to zero
    where
    normalResult =
        NumOrd.maxDnEff effMinmax  -- we do not know the sign of a
            (divUpEff effDiv a bDn)
            (divUpEff effDiv a bUp)
    bUp = convertUpEff effConv b
    bDn = convertDnEff effConv b

mixedDivDnEffByConversion ::
    (Convertible t1 t2, 
     RoundedDivide t2, 
     HasZero t2,  HasInfinities t2,
     NumOrd.PartialComparison t2,
     NumOrd.RoundedLattice t2) =>
    (DivEffortIndicator t2, 
     ConvertEffortIndicator t1 t2,
     (NumOrd.MinmaxEffortIndicator t2, 
      NumOrd.PartialCompareEffortIndicator t2)) ->
    t2 -> t1 -> t2
mixedDivDnEffByConversion (effDiv, effConv, (effMinmax, effComp)) a b = 
    let ?pCompareEffort = effComp in
    case (bDn >=? zero, bUp <=? zero) of
        (Just True, _) -> normalResult 
        (_, Just True) -> normalResult
        _ -> minusInfinity -- b is too close to zero
    where
    normalResult =
        NumOrd.maxDnEff effMinmax  -- we do not know the sign of a
            (divDnEff effDiv a bDn)
            (divDnEff effDiv a bUp)
    bUp = convertUpEff effConv b
    bDn = convertDnEff effConv b

{- properties of mixed multiplication -}

propMixedDivEqualsConvert ::
    (NumOrd.PartialComparison t2, NumOrd.RoundedLattice t2, 
     Convertible t1 t2,
     RoundedMixedDivide t1 t2, RoundedDivide t2,
     HasDistance t2,  Show (Distance t2),  
     NumOrd.PartialComparison (Distance t2), 
     HasInfinities (Distance t2), HasZero (Distance t2),
     Show (MixedDivEffortIndicator t1 t2),
     EffortIndicator (MixedDivEffortIndicator t1 t2),
     Show (ConvertEffortIndicator t1 t2),
     EffortIndicator (ConvertEffortIndicator t1 t2),
     Show (DivEffortIndicator t2),
     EffortIndicator (DivEffortIndicator t2),
     Show (NumOrd.PartialCompareEffortIndicator t2),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t2),
     Show (NumOrd.MinmaxEffortIndicator t2),
     EffortIndicator (NumOrd.MinmaxEffortIndicator t2)
     ) =>
    t1 -> t2 ->
    (DistanceEffortIndicator t2) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t2)) -> 
    (NumOrd.PartialCompareEffortIndicator t2,
     (MixedDivEffortIndicator t1 t2,      
      (DivEffortIndicator t2,
       ConvertEffortIndicator t1 t2,
       NumOrd.MinmaxEffortIndicator t2))) -> 
    t2 -> t1 -> Bool
propMixedDivEqualsConvert sample1 sample2 effortDist effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1Up (effMDiv,_) =
        let (|/^) = mixedDivUpEff effMDiv in e1 |/^ e2
    expr1Dn (effMDiv,_) =
        let (|/.) = mixedDivDnEff effMDiv in e1 |/. e2
    expr2Up (_,(effDiv,effConv,effMinmax)) =
        let (/^) = divUpEff effDiv in
        NumOrd.maxUpEff effMinmax  
            (e1 /^ (convertUpEff effConv e2))
            (e1 /^ (convertDnEff effConv e2))
    expr2Dn (_,(effDiv,effConv,effMinmax)) =
        let (/.) = divDnEff effDiv in
        NumOrd.minDnEff effMinmax  
            (e1 /. (convertUpEff effConv e2))
            (e1 /. (convertDnEff effConv e2))
    
testsUpDnMixedFieldOps (name1, sample1) (name2, sample2) =
    testGroup (name1 ++ " with " ++ name2 ++ ": mixed up/dn rounded ops") $
        [
            testProperty "addition" (propMixedAddEqualsConvert sample1 sample2)
        ,
            testProperty "multiplication" (propMixedMultEqualsConvert sample1 sample2)
        ,
            testProperty "division" (propMixedDivEqualsConvert sample1 sample2)
        ]
    