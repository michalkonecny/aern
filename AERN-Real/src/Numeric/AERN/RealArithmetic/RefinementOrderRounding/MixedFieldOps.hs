{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RefinementOrderRounding.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing 2 types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations mixing 2 types.
    
    This module is hidden and reexported via its parent RefinementOrderRounding. 
-}
module Numeric.AERN.RealArithmetic.RefinementOrderRounding.MixedFieldOps where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion
import Numeric.AERN.RealArithmetic.ExactOps

import Numeric.AERN.Basics.Effort
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class RoundedMixedAdd t tn where
    type MixedAddEffortIndicator t tn
    mixedAddInEff :: MixedAddEffortIndicator t tn -> t -> tn -> t
    mixedAddOutEff :: MixedAddEffortIndicator t tn -> t -> tn -> t
    mixedAddDefaultEffort :: t -> tn -> MixedAddEffortIndicator t tn

mixedAddDefaultEffortByConversion d n = 
        (addDefaultEffort d, convertDefaultEffort n d)

mixedAddInEffByConversion ::
    (Convertible tn t, RoundedAdd t) =>
    (AddEffortIndicator t, ConvertEffortIndicator tn t) ->
    t -> tn -> t
mixedAddInEffByConversion (effAdd, effConv) d n = 
    addInEff effAdd d (convertInEff effConv n)

mixedAddOutEffByConversion ::
    (Convertible tn t, RoundedAdd t) =>
    (AddEffortIndicator t, ConvertEffortIndicator tn t) ->
    t -> tn -> t
mixedAddOutEffByConversion (effAdd, effConv) d n = 
    addOutEff effAdd d (convertOutEff effConv n)


propMixedAddEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
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
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
     RefOrd.PartialCompareEffortIndicator t,
     (MixedAddEffortIndicator t tn,      
      AddEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    t -> tn -> Bool
propMixedAddEqualsConvert sample sampleN effortDistComp initEffort d n =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out
        RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    expr1In (effMAdd,_,_) =
        let (>+<|) = mixedAddInEff effMAdd in d >+<| n
    expr1Out (effMAdd,_,_) =
        let (<+>|) = mixedAddOutEff effMAdd in d <+>| n
    expr2In (_,effAdd,effConv) =
        let (>+<) = addInEff effAdd in  d >+< (convertInEff effConv n)
    expr2Out (_,effAdd,effConv) =
        let (<+>) = addOutEff effAdd in  d <+> (convertOutEff effConv n)


class RoundedMixedMultiply t tn where
    type MixedMultEffortIndicator t tn
    mixedMultDefaultEffort :: t -> tn -> MixedMultEffortIndicator t tn
    mixedMultInEff :: MixedMultEffortIndicator t tn -> t -> tn -> t
    mixedMultOutEff :: MixedMultEffortIndicator t tn -> t -> tn -> t

mixedMultDefaultEffortByConversion d n = 
        (multDefaultEffort d, convertDefaultEffort n d)

mixedMultInEffByConversion ::
    (Convertible tn t, RoundedMultiply t) =>
    (MultEffortIndicator t, ConvertEffortIndicator tn t) ->
    t -> tn -> t
mixedMultInEffByConversion (effMult, effConv) d n = 
    multInEff effMult d (convertInEff effConv n)

mixedMultOutEffByConversion ::
    (Convertible tn t, RoundedMultiply t) =>
    (MultEffortIndicator t, ConvertEffortIndicator tn t) ->
    t -> tn -> t
mixedMultOutEffByConversion (effMult, effConv) d n = 
    multOutEff effMult d (convertOutEff effConv n)


propMixedMultEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
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
     Show (DistanceEffortIndicator t),
     EffortIndicator (DistanceEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
    RefOrd.PartialCompareEffortIndicator t,
     (MixedMultEffortIndicator t tn,      
      MultEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    t -> tn -> Bool
propMixedMultEqualsConvert sample sampleN effortDistComp initEffort d n =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out
        RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    where
    expr1In (effMMult,_,_) =
        let (>*<|) = mixedMultInEff effMMult in d >*<| n
    expr1Out (effMMult,_,_) =
        let (<*>|) = mixedMultOutEff effMMult in d <*>| n
    expr2In (_,effMult,effConv) =
        let (>*<) = multInEff effMult in d >*< (convertInEff effConv n)
    expr2Out (_,effMult,effConv) =
        let (<*>) = multOutEff effMult in d <*> (convertOutEff effConv n)

class RoundedMixedDivide t tn where
    type MixedDivEffortIndicator t tn
    mixedDivDefaultEffort :: t -> tn -> MixedDivEffortIndicator t tn
    mixedDivInEff :: MixedDivEffortIndicator t tn -> t -> tn -> t
    mixedDivOutEff :: MixedDivEffortIndicator t tn -> t -> tn -> t

mixedDivDefaultEffortByConversion d n = 
        (divDefaultEffort d, convertDefaultEffort n d)

mixedDivInEffByConversion ::
    (Convertible tn t, RoundedDivide t) =>
    (DivEffortIndicator t, ConvertEffortIndicator tn t) ->
    t -> tn -> t
mixedDivInEffByConversion (effDiv, effConv) d n = 
    divInEff effDiv d (convertInEff effConv n)

mixedDivOutEffByConversion ::
    (Convertible tn t, RoundedDivide t) =>
    (DivEffortIndicator t, ConvertEffortIndicator tn t) ->
    t -> tn -> t
mixedDivOutEffByConversion (effDiv, effConv) d n = 
    divOutEff effDiv d (convertOutEff effConv n)


propMixedDivEqualsConvert ::
    (RefOrd.PartialComparison t, Convertible tn t,
     RoundedMixedDivide t tn, RoundedDivide t,
     Show t, HasZero t,
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
     EffortIndicator (DistanceEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (NumOrd.PartialCompareEffortIndicator (Distance t)) -> 
    (DistanceEffortIndicator t,
     RefOrd.PartialCompareEffortIndicator t,
     (MixedDivEffortIndicator t tn,      
      DivEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    t -> tn -> Bool
propMixedDivEqualsConvert sample sampleN effortDistComp initEffort@(_,effComp,(_,_,effConv)) d n
    | awayFromZero =
            equalRoundingUpDnImprovement
                expr1In expr1Out expr2In expr2Out
                RefOrd.pLeqEff distanceBetweenEff effortDistComp initEffort
    | otherwise = True
    where
    awayFromZero =
        case (convertOutEff effConv n, convertInEff effConv n) of
            (nOut, nIn) ->
                let ?pCompareEffort = effComp in
                case (nOut ⊑? zero, zero ⊑? nIn) of
                    (Just False, Just False) -> True
                    _ -> False && (null [d, nOut, nIn]) -- type of e2Up, e2Dn...
    expr1In (effMDiv,_,_) =
        let (>/<|) = mixedDivInEff effMDiv in d >/<| n
    expr1Out (effMDiv,_,_) =
        let (</>|) = mixedDivOutEff effMDiv in d </>| n
    expr2In (_,effDiv,effConv) =
        let (>/<) = divInEff effDiv in d >/< (convertInEff effConv n)
    expr2Out (_,effDiv,effConv) =
        let (</>) = divOutEff effDiv in d </> (convertOutEff effConv n)

    
testsInOutMixedFieldOps (name, sample) (nameN, sampleN) =
    testGroup (name ++ " with " ++ nameN ++ ": mixed in/out rounded ops") $
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
        
        