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

import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.Laws 
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.NumericOrder.OpsImplicitEffort

import Control.Exception
import Data.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class RoundedMixedAdd t tn where
    type MixedAddEffortIndicator t tn
    mixedAddDefaultEffort :: t -> tn -> MixedAddEffortIndicator t tn
    mixedAddUpEff :: MixedAddEffortIndicator t tn -> t -> tn -> t
    mixedAddDnEff :: MixedAddEffortIndicator t tn -> t -> tn -> t

{- tools to easily make a RoundedMixedAdd instance 
   via the composition of conversion and homogeneous addition -}

type MixedAddEffortIndicatorByConversion t tn =
        (AddEffortIndicator t, ConvertEffortIndicator tn t)

mixedAddDefaultEffortByConversion d n = 
        (addDefaultEffort d, convertDefaultEffort n d)

mixedAddUpEffByConversion ::
    (Convertible tn t, RoundedAdd t, Show tn) =>
    (AddEffortIndicator t, ConvertEffortIndicator tn t) ->
    t -> tn -> t
mixedAddUpEffByConversion (effAdd, effConv) d n = 
    addUpEff effAdd nUp d
    where
    nUp = 
        case convertUpEff effConv n of
            (Just nUp) -> nUp
            _ -> throw $ AERNException $ 
                        "conversion failed during mixed addition: n = " ++ show n

mixedAddDnEffByConversion ::
    (Convertible tn t, RoundedAdd t, Show tn) =>
    (AddEffortIndicator t, ConvertEffortIndicator tn t) ->
    t -> tn -> t
mixedAddDnEffByConversion (effAdd, effConv) d n = 
    addDnEff effAdd nDn d
    where
    nDn = 
        case convertDnEff effConv n of
            (Just nDn) -> nDn
            _ -> throw $ AERNException $ 
                        "conversion failed during mixed addition: n = " ++ show n

{- properties of mixed addition -}

propMixedAddEqualsConvert ::
    (NumOrd.PartialComparison t, Convertible tn t,
     RoundedMixedAdd t tn, RoundedAdd t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), 
     HasInfinities (Distance t), HasZero (Distance t),
     Show (MixedAddEffortIndicator t tn),
     EffortIndicator (MixedAddEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
     NumOrd.PartialCompareEffortIndicator t,
     (MixedAddEffortIndicator t tn,      
      AddEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    t -> tn -> Bool
propMixedAddEqualsConvert sampleN sample effortDistComp initEffort d n =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    expr1Up (effMAdd,_,_) =
        let (+^|) = mixedAddUpEff effMAdd in d +^| n
    expr1Dn (effMAdd,_,_) =
        let (+.|) = mixedAddDnEff effMAdd in d +.| n
    expr2Up (_,effAdd,effConv) =
        let (+^) = addUpEff effAdd in d +^ (fromJust $ convertUpEff effConv n)
    expr2Dn (_,effAdd,effConv) =
        let (+.) = addDnEff effAdd in d +. (fromJust $ convertDnEff effConv n)

class RoundedMixedMultiply t tn where
    type MixedMultEffortIndicator t tn
    mixedMultDefaultEffort :: t -> tn -> MixedMultEffortIndicator t tn
    mixedMultUpEff :: MixedMultEffortIndicator t tn -> t -> tn -> t
    mixedMultDnEff :: MixedMultEffortIndicator t tn -> t -> tn -> t

{- tools to easily make a RoundedMixedMultiply instance 
   via the composition of conversion and homogeneous addition -}

type MixedMultEffortIndicatorByConversion t tn =
        (MultEffortIndicator t, 
         ConvertEffortIndicator tn t,
         NumOrd.MinmaxEffortIndicator t)

mixedMultDefaultEffortByConversion d n = 
        (addDefaultEffort d, 
         convertDefaultEffort n d,
         NumOrd.minmaxDefaultEffort d)

mixedMultUpEffByConversion ::
    (Convertible tn t, RoundedMultiply t, NumOrd.RoundedLattice t, Show tn) =>
    (MultEffortIndicator t, 
     ConvertEffortIndicator tn t,
     NumOrd.MinmaxEffortIndicator t) ->
    t -> tn -> t
mixedMultUpEffByConversion (effMult, effConv, effMinmax) d n =
    NumOrd.maxUpEff effMinmax
    (multUpEff effMult d nDn)
    (multUpEff effMult d nUp)
    where
    (nUp, nDn) = 
        case (convertUpEff effConv n, convertDnEff effConv n) of
            (Just nUp, Just nDn) -> (nUp, nDn)
            _ -> throw $ AERNException $ 
                        "conversion failed during mixed multiplication: n = " ++ show n

mixedMultDnEffByConversion ::
    (Convertible tn t, RoundedMultiply t, NumOrd.RoundedLattice t, Show tn) =>
    (MultEffortIndicator t, 
     ConvertEffortIndicator tn t,
     NumOrd.MinmaxEffortIndicator t) ->
    t -> tn -> t
mixedMultDnEffByConversion (effMult, effConv, effMinmax) d n =
    NumOrd.minDnEff effMinmax
    (multDnEff effMult d nDn)
    (multDnEff effMult d nUp)
    where
    (nUp, nDn) = 
        case (convertUpEff effConv n, convertDnEff effConv n) of
            (Just nUp, Just nDn) -> (nUp, nDn)
            _ -> throw $ AERNException $ 
                        "conversion failed during mixed multiplication: n = " ++ show n


{- properties of mixed multiplication -}

propMixedMultEqualsConvert ::
    (NumOrd.PartialComparison t, NumOrd.RoundedLattice t, 
     Convertible tn t,
     RoundedMixedMultiply t tn, RoundedMultiply t,
     Show t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), 
     HasInfinities (Distance t), HasZero (Distance t),
     Show (MixedMultEffortIndicator t tn),
     EffortIndicator (MixedMultEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (NumOrd.MinmaxEffortIndicator t),
     EffortIndicator (NumOrd.MinmaxEffortIndicator t),
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t, 
     NumOrd.PartialCompareEffortIndicator t,
     (MixedMultEffortIndicator t tn,      
      (MultEffortIndicator t,
       ConvertEffortIndicator tn t,
       NumOrd.MinmaxEffortIndicator t))) -> 
    t -> tn -> Bool
propMixedMultEqualsConvert sample sampleN effortDistComp initEffort d n =
    equalRoundingUpDnImprovement
        expr1Up expr1Dn expr2Up expr2Dn 
        NumOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    expr1Up (effMMult,_) =
        let (*^|) = mixedMultUpEff effMMult in d *^| n
    expr1Dn (effMMult,_) =
        let (*.|) = mixedMultDnEff effMMult in d *.| n
    expr2Up (_,(effMult,effConv,effMinmax)) =
        let (*^) = multUpEff effMult in
        NumOrd.maxUpEff effMinmax  
            (d *^ (fromJust $ convertUpEff effConv n))
            (d *^ (fromJust $ convertDnEff effConv n))
    expr2Dn (_,(effMult,effConv,effMinmax)) =
        let (*.) = multDnEff effMult in
        NumOrd.minDnEff effMinmax  
            (d *. (fromJust $ convertUpEff effConv n))
            (d *. (fromJust $ convertDnEff effConv n))

class RoundedMixedDivide t tn where
    type MixedDivEffortIndicator t tn
    mixedDivDefaultEffort :: t -> tn -> MixedDivEffortIndicator t tn
    mixedDivUpEff :: MixedDivEffortIndicator t tn -> t -> tn -> t
    mixedDivDnEff :: MixedDivEffortIndicator t tn -> t -> tn -> t

{- tools to easily make a RoundedMixedDivide instance 
   via the composition of conversion and homogeneous addition -}

type MixedDivEffortIndicatorByConversion t tn =
        (DivEffortIndicator t, 
         ConvertEffortIndicator tn t,
         (NumOrd.MinmaxEffortIndicator t,
          NumOrd.PartialCompareEffortIndicator t))

mixedDivDefaultEffortByConversion d n = 
        (addDefaultEffort d, 
         convertDefaultEffort n d,
         (NumOrd.minmaxDefaultEffort d,
          NumOrd.pCompareDefaultEffort d))

mixedDivUpEffByConversion ::
    (Convertible tn t, 
     RoundedDivide t, 
     HasZero t,  HasInfinities t,
     NumOrd.PartialComparison t,
     NumOrd.RoundedLattice t,
     Show tn) =>
    (DivEffortIndicator t, 
     ConvertEffortIndicator tn t,
     (NumOrd.MinmaxEffortIndicator t, 
      NumOrd.PartialCompareEffortIndicator t)) ->
    t -> tn -> t
mixedDivUpEffByConversion (effDiv, effConv, (effMinmax, effComp)) d n =
    let ?pCompareEffort = effComp in
    case (nDn >=? zero, nUp <=? zero) of
        (Just True, _) -> normalResult 
        (_, Just True) -> normalResult
        _ -> plusInfinity -- b is too close to zero
    where
    normalResult =
        NumOrd.maxDnEff effMinmax  -- we do not know the sign of a
            (divUpEff effDiv d nDn)
            (divUpEff effDiv d nUp)
    (nUp, nDn) = 
        case (convertUpEff effConv n, convertDnEff effConv n) of
            (Just nUp, Just nDn) -> (nUp, nDn)
            _ -> throw $ AERNException $ 
                        "conversion failed during mixed division: n = " ++ show n

mixedDivDnEffByConversion ::
    (Convertible tn t, 
     RoundedDivide t, 
     HasZero t,  HasInfinities t,
     NumOrd.PartialComparison t,
     NumOrd.RoundedLattice t,
     Show tn) =>
    (DivEffortIndicator t, 
     ConvertEffortIndicator tn t,
     (NumOrd.MinmaxEffortIndicator t, 
      NumOrd.PartialCompareEffortIndicator t)) ->
    t -> tn -> t
mixedDivDnEffByConversion (effDiv, effConv, (effMinmax, effComp)) d n = 
    let ?pCompareEffort = effComp in
    case (nDn >=? zero, nUp <=? zero) of
        (Just True, _) -> normalResult 
        (_, Just True) -> normalResult
        _ -> minusInfinity -- b is too close to zero
    where
    normalResult =
        NumOrd.minDnEff effMinmax  -- we do not know the sign of a
            (divDnEff effDiv d nDn)
            (divDnEff effDiv d nUp)
    (nUp, nDn) = 
        case (convertUpEff effConv n, convertDnEff effConv n) of
            (Just nUp, Just nDn) -> (nUp, nDn)
            _ -> throw $ AERNException $ 
                        "conversion failed during mixed division: n = " ++ show n

{- properties of mixed multiplication -}

propMixedDivEqualsConvert ::
    (NumOrd.PartialComparison t, NumOrd.RoundedLattice t, 
     Convertible tn t,
     RoundedMixedDivide t tn, RoundedDivide t,
     Show t, 
     HasZero t,
     HasDistance t,  Show (Distance t),  
     NumOrd.PartialComparison (Distance t), 
     HasInfinities (Distance t), HasZero (Distance t),
     Show (MixedDivEffortIndicator t tn),
     EffortIndicator (MixedDivEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (DistanceEffortIndicator t),
     Show (NumOrd.MinmaxEffortIndicator t),
     EffortIndicator (NumOrd.MinmaxEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (NumOrd.PartialCompareEffortIndicator t),
     EffortIndicator (NumOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
     NumOrd.PartialCompareEffortIndicator t,
     (MixedDivEffortIndicator t tn,      
      (DivEffortIndicator t,
       ConvertEffortIndicator tn t,
       NumOrd.MinmaxEffortIndicator t))) -> 
    t -> tn -> Bool
propMixedDivEqualsConvert sample sampleN effortDistComp initEffort@(_,effComp,(_,(_,effConv,_))) d n
    | awayFromZero =
            equalRoundingUpDnImprovement
                expr1Up expr1Dn expr2Up expr2Dn 
                NumOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    | otherwise = True
    where
    awayFromZero =
        case (convertDnEff effConv n, convertUpEff effConv n) of
            (Just nDn, Just nUp) ->
                let ?pCompareEffort = effComp in
                case (nUp <? zero, zero <? nDn) of
                    (Just True, _) -> True
                    (_, Just True) -> True
                    _ -> False && (null [d, nUp, nDn]) -- type of nUp, nDn...
    expr1Up (effMDiv,_) =
        let (/^|) = mixedDivUpEff effMDiv in d /^| n
    expr1Dn (effMDiv,_) =
        let (/.|) = mixedDivDnEff effMDiv in d /.| n
    expr2Up (_,(effDiv,effConv,effMinmax)) =
        let (/^) = divUpEff effDiv in
        NumOrd.maxUpEff effMinmax  
            (d /^ (fromJust $ convertUpEff effConv n))
            (d /^ (fromJust $ convertDnEff effConv n))
    expr2Dn (_,(effDiv,effConv,effMinmax)) =
        let (/.) = divDnEff effDiv in
        NumOrd.minDnEff effMinmax  
            (d /. (fromJust $ convertUpEff effConv n))
            (d /. (fromJust $ convertDnEff effConv n))
    
testsUpDnMixedFieldOps (name, sample) (nameN, sampleN) =
    testGroup (name ++ " with " ++ nameN ++ ": mixed up/dn rounded ops") $
        [
            testProperty "addition" (propMixedAddEqualsConvert sample sampleN)
        ,
            testProperty "multiplication" (propMixedMultEqualsConvert sample sampleN)
        ,
            testProperty "division" (propMixedDivEqualsConvert sample sampleN)
        ]

class (RoundedMixedAdd t tn, RoundedMixedMultiply t tn) => RoundedMixedRing t tn

class (RoundedMixedRing t tn, RoundedMixedDivide t tn) => RoundedMixedField t tn
    where
    type MixedFieldOpsEffortIndicator t tn
    mixedFieldOpsDefaultEffort :: t -> tn -> MixedFieldOpsEffortIndicator t tn
    mxfldEffortAdd :: t -> tn -> MixedFieldOpsEffortIndicator t tn -> MixedAddEffortIndicator t tn
    mxfldEffortMult :: t -> tn -> MixedFieldOpsEffortIndicator t tn -> MixedMultEffortIndicator t tn
    mxfldEffortDiv :: t -> tn -> MixedFieldOpsEffortIndicator t tn -> MixedDivEffortIndicator t tn

    