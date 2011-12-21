{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Numeric.AERN.Basics.Exception (HasLegalValues)
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

class RoundedMixedAddEffort t tn where
    type MixedAddEffortIndicator t tn
    mixedAddDefaultEffort :: t -> tn -> MixedAddEffortIndicator t tn

class (RoundedMixedAddEffort t tn) => RoundedMixedAdd t tn where
    mixedAddInEff :: MixedAddEffortIndicator t tn -> t -> tn -> t
    mixedAddOutEff :: MixedAddEffortIndicator t tn -> t -> tn -> t

-- The following would prevent one from defining a generic instance
-- for an interval based on its endpoints: 
--
--instance (RoundedAddEffort t) => (RoundedMixedAddEffort t t)
--    where
--    type MixedAddEffortIndicator t t = AddEffortIndicator t
--    mixedAddDefaultEffort _ sample = addDefaultEffort sample 
--instance (RoundedAdd t) => (RoundedMixedAdd t t)
--    where
--    mixedAddOutEff = addOutEff
--    mixedAddInEff = addInEff

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
     Show t, HasLegalValues t,
     Show (MixedAddEffortIndicator t tn),
     EffortIndicator (MixedAddEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (RefOrd.PartialCompareEffortIndicator t,
     (MixedAddEffortIndicator t tn,      
      AddEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    tn -> Bool
propMixedAddEqualsConvert sample sampleN initEffort 
        (RefOrd.UniformlyOrderedSingleton d) n =
    equalRoundingUpDn "mixed addition by conversion"
        expr1In expr1Out expr2In expr2Out
        RefOrd.pLeqEff initEffort
    where
    expr1In (effMAdd,_,_) =
        let (>+<|) = mixedAddInEff effMAdd in d >+<| n
    expr1Out (effMAdd,_,_) =
        let (<+>|) = mixedAddOutEff effMAdd in d <+>| n
    expr2In (_,effAdd,effConv) =
        let (>+<) = addInEff effAdd in  d >+< (convertInEff effConv n)
    expr2Out (_,effAdd,effConv) =
        let (<+>) = addOutEff effAdd in  d <+> (convertOutEff effConv n)


class RoundedMixedMultiplyEffort t tn where
    type MixedMultEffortIndicator t tn
    mixedMultDefaultEffort :: t -> tn -> MixedMultEffortIndicator t tn

class (RoundedMixedMultiplyEffort t tn) =>  RoundedMixedMultiply t tn where
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
     Show t, HasLegalValues t,
     Show (MixedMultEffortIndicator t tn),
     EffortIndicator (MixedMultEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (RefOrd.PartialCompareEffortIndicator t,
      (MixedMultEffortIndicator t tn,      
       MultEffortIndicator t,
       ConvertEffortIndicator tn t)) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    tn -> Bool
propMixedMultEqualsConvert sample sampleN initEffort 
        (RefOrd.UniformlyOrderedSingleton d) n =
    equalRoundingUpDn "mixed multiplication by conversion"
        expr1In expr1Out expr2In expr2Out
        RefOrd.pLeqEff initEffort
    where
    expr1In (effMMult,_,_) =
        let (>*<|) = mixedMultInEff effMMult in d >*<| n
    expr1Out (effMMult,_,_) =
        let (<*>|) = mixedMultOutEff effMMult in d <*>| n
    expr2In (_,effMult,effConv) =
        let (>*<) = multInEff effMult in d >*< (convertInEff effConv n)
    expr2Out (_,effMult,effConv) =
        let (<*>) = multOutEff effMult in d <*> (convertOutEff effConv n)

class RoundedMixedDivideEffort t tn where
    type MixedDivEffortIndicator t tn
    mixedDivDefaultEffort :: t -> tn -> MixedDivEffortIndicator t tn

class (RoundedMixedDivideEffort t tn) => RoundedMixedDivide t tn where
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
     Show t, HasZero t, HasLegalValues t,
     Show (MixedDivEffortIndicator t tn),
     EffortIndicator (MixedDivEffortIndicator t tn),
     Show (ConvertEffortIndicator tn t),
     EffortIndicator (ConvertEffortIndicator tn t),
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t -> tn ->
    (RefOrd.PartialCompareEffortIndicator t,
     (MixedDivEffortIndicator t tn,      
      DivEffortIndicator t,
      ConvertEffortIndicator tn t)) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> 
    tn -> Bool
propMixedDivEqualsConvert sample sampleN initEffort@(effComp,(_,_,effConv)) 
        (RefOrd.UniformlyOrderedSingleton d) n
    =
    equalRoundingUpDn "mixed division by conversion"
        expr1In expr1Out expr2In expr2Out
        RefOrd.pLeqEff initEffort
    where
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

class (RoundedMixedAddEffort t tn, RoundedMixedMultiplyEffort t tn) => 
    RoundedMixedRingEffort t tn
    where
    type MixedRingOpsEffortIndicator t tn
    mixedRingOpsDefaultEffort :: t -> tn -> MixedRingOpsEffortIndicator t tn
    mxringEffortAdd :: t -> tn -> MixedRingOpsEffortIndicator t tn -> MixedAddEffortIndicator t tn
    mxringEffortMult :: t -> tn -> MixedRingOpsEffortIndicator t tn -> MixedMultEffortIndicator t tn

class (RoundedMixedAdd t tn, RoundedMixedMultiply t tn, RoundedMixedRingEffort t tn) => 
    RoundedMixedRing t tn

class (RoundedMixedRingEffort t tn, RoundedMixedDivideEffort t tn) => 
    RoundedMixedFieldEffort t tn
    where
    type MixedFieldOpsEffortIndicator t tn
    mixedFieldOpsDefaultEffort :: t -> tn -> MixedFieldOpsEffortIndicator t tn
    mxfldEffortAdd :: t -> tn -> MixedFieldOpsEffortIndicator t tn -> MixedAddEffortIndicator t tn
    mxfldEffortMult :: t -> tn -> MixedFieldOpsEffortIndicator t tn -> MixedMultEffortIndicator t tn
    mxfldEffortDiv :: t -> tn -> MixedFieldOpsEffortIndicator t tn -> MixedDivEffortIndicator t tn
        
class (RoundedMixedRing t tn, RoundedMixedDivide t tn, RoundedMixedFieldEffort t tn) => 
    RoundedMixedField t tn
        