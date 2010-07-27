{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class RoundedMixedAdd s t where
    type MixedAddEffortIndicator s t
    mixedAddInEff :: MixedAddEffortIndicator s t -> s -> t -> t
    mixedAddOutEff :: MixedAddEffortIndicator s t -> s -> t -> t
    mixedAddDefaultEffort :: s -> t -> MixedAddEffortIndicator s t

mixedAddDefaultEffortByConversion n d = 
        (addDefaultEffort d, convertDefaultEffort n d)

mixedAddInEffByConversion ::
    (Convertible t1 t2, RoundedAdd t2) =>
    (AddEffortIndicator t2, ConvertEffortIndicator t1 t2) ->
    t1 -> t2 -> t2
mixedAddInEffByConversion (effAdd, effConv) a b = 
    addInEff effAdd (convertInEff effConv a) b

mixedAddOutEffByConversion ::
    (Convertible t1 t2, RoundedAdd t2) =>
    (AddEffortIndicator t2, ConvertEffortIndicator t1 t2) ->
    t1 -> t2 -> t2
mixedAddOutEffByConversion (effAdd, effConv) a b = 
    addOutEff effAdd (convertOutEff effConv a) b


propMixedAddEqualsConvert ::
    (RefOrd.PartialComparison t2, Convertible t1 t2,
     RoundedMixedAdd t1 t2, RoundedAdd t2,
     Show t2,
     HasDistance t2,  Show (Distance t2),  
     NumOrd.PartialComparison (Distance t2), 
     HasInfinities (Distance t2), HasZero (Distance t2),
     Show (MixedAddEffortIndicator t1 t2),
     EffortIndicator (MixedAddEffortIndicator t1 t2),
     Show (ConvertEffortIndicator t1 t2),
     EffortIndicator (ConvertEffortIndicator t1 t2),
     Show (AddEffortIndicator t2),
     EffortIndicator (AddEffortIndicator t2),
     Show (RefOrd.PartialCompareEffortIndicator t2),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t2)
     ) =>
    t1 -> t2 ->
    (DistanceEffortIndicator t2) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t2)) -> 
    (RefOrd.PartialCompareEffortIndicator t2,
     (MixedAddEffortIndicator t1 t2,      
      AddEffortIndicator t2,
      ConvertEffortIndicator t1 t2)) -> 
    t1 -> t2 -> Bool
propMixedAddEqualsConvert sample1 sample2 effortDist effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out
        RefOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1In (effMAdd,_,_) =
        let (>|+<) = mixedAddInEff effMAdd in e1 >|+< e2
    expr1Out (effMAdd,_,_) =
        let (<|+>) = mixedAddOutEff effMAdd in e1 <|+> e2
    expr2In (_,effAdd,effConv) =
        let (>+<) = addInEff effAdd in (convertInEff effConv e1) >+< e2
    expr2Out (_,effAdd,effConv) =
        let (<+>) = addOutEff effAdd in (convertOutEff effConv e1) <+> e2


class RoundedMixedMultiply s t where
    type MixedMultEffortIndicator s t
    mixedMultInEff :: MixedMultEffortIndicator s t -> s -> t -> t
    mixedMultOutEff :: MixedMultEffortIndicator s t -> s -> t -> t
    mixedMultDefaultEffort :: s -> t -> MixedMultEffortIndicator s t

mixedMultDefaultEffortByConversion n d = 
        (multDefaultEffort d, convertDefaultEffort n d)

mixedMultInEffByConversion ::
    (Convertible t1 t2, RoundedMultiply t2) =>
    (MultEffortIndicator t2, ConvertEffortIndicator t1 t2) ->
    t1 -> t2 -> t2
mixedMultInEffByConversion (effMult, effConv) a b = 
    multInEff effMult (convertInEff effConv a) b

mixedMultOutEffByConversion ::
    (Convertible t1 t2, RoundedMultiply t2) =>
    (MultEffortIndicator t2, ConvertEffortIndicator t1 t2) ->
    t1 -> t2 -> t2
mixedMultOutEffByConversion (effMult, effConv) a b = 
    multOutEff effMult (convertOutEff effConv a) b


propMixedMultEqualsConvert ::
    (RefOrd.PartialComparison t2, Convertible t1 t2,
     RoundedMixedMultiply t1 t2, RoundedMultiply t2,
     Show t2,
     HasDistance t2,  Show (Distance t2),  
     NumOrd.PartialComparison (Distance t2), 
     HasInfinities (Distance t2), HasZero (Distance t2),
     Show (MixedMultEffortIndicator t1 t2),
     EffortIndicator (MixedMultEffortIndicator t1 t2),
     Show (ConvertEffortIndicator t1 t2),
     EffortIndicator (ConvertEffortIndicator t1 t2),
     Show (MultEffortIndicator t2),
     EffortIndicator (MultEffortIndicator t2),
     Show (RefOrd.PartialCompareEffortIndicator t2),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t2)
     ) =>
    t1 -> t2 ->
    (DistanceEffortIndicator t2) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t2)) -> 
    (RefOrd.PartialCompareEffortIndicator t2,
     (MixedMultEffortIndicator t1 t2,      
      MultEffortIndicator t2,
      ConvertEffortIndicator t1 t2)) -> 
    t1 -> t2 -> Bool
propMixedMultEqualsConvert sample1 sample2 effortDist effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out
        RefOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1In (effMMult,_,_) =
        let (>|*<) = mixedMultInEff effMMult in e1 >|*< e2
    expr1Out (effMMult,_,_) =
        let (<|*>) = mixedMultOutEff effMMult in e1 <|*> e2
    expr2In (_,effMult,effConv) =
        let (>*<) = multInEff effMult in (convertInEff effConv e1) >*< e2
    expr2Out (_,effMult,effConv) =
        let (<*>) = multOutEff effMult in (convertOutEff effConv e1) <*> e2

class (RoundedMixedAdd s t, RoundedMixedMultiply s t) => RoundedMixedRing s t

class RoundedMixedDivide s t where
    type MixedDivEffortIndicator s t
    mixedDivDefaultEffort :: s -> t -> MixedDivEffortIndicator s t
    mixedDivInEff :: MixedDivEffortIndicator s t -> t -> s -> t
    mixedDivOutEff :: MixedDivEffortIndicator s t -> t -> s -> t

mixedDivDefaultEffortByConversion n d = 
        (divDefaultEffort d, convertDefaultEffort n d)

mixedDivInEffByConversion ::
    (Convertible t1 t2, RoundedDivide t2) =>
    (DivEffortIndicator t2, ConvertEffortIndicator t1 t2) ->
    t2 -> t1 -> t2
mixedDivInEffByConversion (effDiv, effConv) a b = 
    divInEff effDiv a (convertInEff effConv b)

mixedDivOutEffByConversion ::
    (Convertible t1 t2, RoundedDivide t2) =>
    (DivEffortIndicator t2, ConvertEffortIndicator t1 t2) ->
    t2 -> t1 -> t2
mixedDivOutEffByConversion (effDiv, effConv) a b = 
    divOutEff effDiv a (convertOutEff effConv b)


propMixedDivEqualsConvert ::
    (RefOrd.PartialComparison t2, Convertible t1 t2,
     RoundedMixedDivide t1 t2, RoundedDivide t2,
     Show t2,
     HasDistance t2,  Show (Distance t2),  
     NumOrd.PartialComparison (Distance t2), 
     HasInfinities (Distance t2), HasZero (Distance t2),
     Show (MixedDivEffortIndicator t1 t2),
     EffortIndicator (MixedDivEffortIndicator t1 t2),
     Show (ConvertEffortIndicator t1 t2),
     EffortIndicator (ConvertEffortIndicator t1 t2),
     Show (DivEffortIndicator t2),
     EffortIndicator (DivEffortIndicator t2),
     Show (RefOrd.PartialCompareEffortIndicator t2),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t2)
     ) =>
    t1 -> t2 ->
    (DistanceEffortIndicator t2) -> 
    (NumOrd.PartialCompareEffortIndicator (Distance t2)) -> 
    (RefOrd.PartialCompareEffortIndicator t2,
     (MixedDivEffortIndicator t1 t2,      
      DivEffortIndicator t2,
      ConvertEffortIndicator t1 t2)) -> 
    t2 -> t1 -> Bool
propMixedDivEqualsConvert sample1 sample2 effortDist effortDistComp initEffort e1 e2 =
    equalRoundingUpDnImprovement
        expr1In expr1Out expr2In expr2Out
        RefOrd.pLeqEff (distanceBetweenEff effortDist) effortDistComp initEffort
    where
    expr1In (effMDiv,_,_) =
        let (>|/<) = mixedDivInEff effMDiv in e1 >|/< e2
    expr1Out (effMDiv,_,_) =
        let (<|/>) = mixedDivOutEff effMDiv in e1 <|/> e2
    expr2In (_,effDiv,effConv) =
        let (>/<) = divInEff effDiv in e1 >/< (convertInEff effConv e2)
    expr2Out (_,effDiv,effConv) =
        let (</>) = divOutEff effDiv in e1 </> (convertOutEff effConv e2)

    
testsInOutMixedFieldOps (name1, sample1) (name2, sample2) =
    testGroup (name1 ++ " with " ++ name2 ++ ": mixed in/out rounded ops") $
        [
            testProperty "addition" (propMixedAddEqualsConvert sample1 sample2)
        ,
            testProperty "multiplication" (propMixedMultEqualsConvert sample1 sample2)
        ,
            testProperty "division" (propMixedDivEqualsConvert sample1 sample2)
        ]

--data MixedFieldOpsEffortIndicator t1 t2 =
--        MixedFieldOpsEffortIndicator
--        {
--           mxfldEffortAdd :: MixedAddEffortIndicator t1 t2,
--           mxfldEffortMult :: MixedMultEffortIndicator t1 t2,
--           mxfldEffortDiv :: MixedDivEffortIndicator t1 t2
--        }
--
--mixedFieldOpsDefaultEffort a1 a2 =
--        MixedFieldOpsEffortIndicator
--        {
--           mxfldEffortAdd = mixedAddDefaultEffort a1 a2,
--           mxfldEffortMult = mixedMultDefaultEffort a1 a2,
--           mxfldEffortDiv = mixedDivDefaultEffort a1 a2
--        }

class (RoundedMixedRing s t, RoundedMixedDivide s t) => RoundedMixedField s t
    where
    type MixedFieldOpsEffortIndicator s t
    mixedFieldOpsDefaultEffort :: s -> t -> MixedFieldOpsEffortIndicator s t
    mxfldEffortAdd :: s -> t -> MixedFieldOpsEffortIndicator s t -> MixedAddEffortIndicator s t
    mxfldEffortMult :: s -> t -> MixedFieldOpsEffortIndicator s t -> MixedMultEffortIndicator s t
    mxfldEffortDiv :: s -> t -> MixedFieldOpsEffortIndicator s t -> MixedDivEffortIndicator s t
        
        