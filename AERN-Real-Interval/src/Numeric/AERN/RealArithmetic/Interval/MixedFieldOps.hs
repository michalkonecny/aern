{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing 2 types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations mixing an interval and another type.
    
    This module is hidden and reexported via its parent Interval. 
-}

module Numeric.AERN.RealArithmetic.Interval.MixedFieldOps 
()
where

import Numeric.AERN.Basics.Interval

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.ExactOps
import Numeric.AERN.RealArithmetic.Interval.FieldOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

instance (ArithUpDn.RoundedMixedAddEffort e tn) => 
    RoundedMixedAddEffort (Interval e) tn 
    where
    type MixedAddEffortIndicator (Interval e) tn = ArithUpDn.MixedAddEffortIndicator e tn
    mixedAddDefaultEffort (Interval l r) n = ArithUpDn.mixedAddDefaultEffort l n

instance (ArithUpDn.RoundedMixedAdd e tn) => 
    RoundedMixedAdd (Interval e) tn 
    where
    mixedAddInEff effort (Interval l2 r2) n =
        Interval
            (ArithUpDn.mixedAddUpEff effort l2 n)
            (ArithUpDn.mixedAddDnEff effort r2 n)
    mixedAddOutEff effort (Interval l2 r2) n =
        Interval 
            (ArithUpDn.mixedAddDnEff effort l2 n)
            (ArithUpDn.mixedAddUpEff effort r2 n)


instance (ArithUpDn.RoundedMixedMultiplyEffort e tn,
          NumOrd.PartialComparison tn, NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e) => 
    RoundedMixedMultiplyEffort (Interval e) tn 
    where
    type MixedMultEffortIndicator (Interval e) tn = 
        ((NumOrd.PartialCompareEffortIndicator tn, 
          NumOrd.PartialCompareEffortIndicator e), 
         NumOrd.MinmaxEffortIndicator e,
         ArithUpDn.MixedMultEffortIndicator e tn)
    mixedMultDefaultEffort (Interval l r) n = 
        ((NumOrd.pCompareDefaultEffort n, 
          NumOrd.pCompareDefaultEffort l), 
         NumOrd.minmaxDefaultEffort l,
         ArithUpDn.mixedMultDefaultEffort l n) 

instance (ArithUpDn.RoundedMixedMultiply e tn,
          HasZero tn, HasZero e,
          NumOrd.PartialComparison tn, NumOrd.PartialComparison e,
          NumOrd.RoundedLattice e) => 
    RoundedMixedMultiply (Interval e) tn 
    where
    mixedMultInEff ((effortCompS, effortCompE), effortMinmax, effortMult) i n =
        fromEndpoints $
        multiplySingletonWithInterval 
            (pNonnegNonposEff effortCompS)
            (pNonnegNonposEff effortCompE)
            (flip $ ArithUpDn.mixedMultUpEff effortMult)
            (flip $ ArithUpDn.mixedMultDnEff effortMult) 
            (NumOrd.maxUpEff effortMinmax) 
            (NumOrd.minDnEff effortMinmax)
            n i
    mixedMultOutEff ((effortCompS, effortCompE), effortMinmax, effortMult) i n =
        fromEndpoints $
        multiplySingletonWithInterval 
            (pNonnegNonposEff effortCompS)
            (pNonnegNonposEff effortCompE)
            (flip $ ArithUpDn.mixedMultDnEff effortMult) 
            (flip $ ArithUpDn.mixedMultUpEff effortMult)
            (NumOrd.minDnEff effortMinmax)
            (NumOrd.maxUpEff effortMinmax) 
            n i

multiplySingletonWithInterval 
        sNonnegNonpos iNonnegNonpos timesL timesR 
        combineL combineR
        s1 (Interval l2 r2) =
    let _ = [combineL, combineR] in
        case (sNonnegNonpos s1, -- sign of s1 
              iNonnegNonpos l2, -- sign of l2
              iNonnegNonpos r2 -- sign of r2 
             ) of
             
            -- s1 is zero
            ((Just True, Just True), _, _) -> 
                (z, z)
 
            -- s1 non negative
            ((Just True, _), _, _) -> 
                (s1 `timesL` l2, s1 `timesR` r2)
            
            -- s1 non positive
            ((_, Just True), _, _) -> 
                (s1 `timesL` r2, s1 `timesR` l2)

            -- nothing known about s1, i2 positive
            (_, (Just True, _), (Just True, _)) -> 
                ((s1 `timesL` r2) `combineL` (s1 `timesL` l2), 
                 (s1 `timesR` r2) `combineR` (s1 `timesR` l2))

            -- nothing known about s1, i2 negative
            (_, (_, Just True), (_, Just True)) -> 
                ((s1 `timesL` r2) `combineL` (s1 `timesL` l2), 
                 (s1 `timesR` r2) `combineR` (s1 `timesR` l2))

            -- both s1 and i2 are around zero
            _ ->
                ((s1 `timesL` l2) `combineL` (s1 `timesL` r2) `combineL` z,
                 (s1 `timesR` l2) `combineR` (s1 `timesR` r2) `combineR` z) 
                -- need to include zero to account for 
                -- consistent vs anti-consistent cases giving constant 0
        where
        z = zero l2
        
        
instance (RoundedDivideEffort (Interval e),
          Convertible tn (Interval e)) => 
    RoundedMixedDivideEffort (Interval e) tn 
    where
    type MixedDivEffortIndicator (Interval e) tn =
        (DivEffortIndicator (Interval e), 
         ConvertEffortIndicator tn (Interval e))
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion

instance (RoundedDivide (Interval e),
          Convertible tn (Interval e)) => 
    RoundedMixedDivide (Interval e) tn 
    where
    mixedDivInEff = mixedDivInEffByConversion
    mixedDivOutEff = mixedDivOutEffByConversion
    
instance (RoundedMixedAddEffort (Interval e) tn,
          RoundedMixedMultiplyEffort (Interval e) tn, 
          NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e,
          NumOrd.PartialComparison tn,
          ArithUpDn.RoundedMixedRingEffort e tn) => 
        RoundedMixedRingEffort (Interval e) tn
    where
    type MixedRingOpsEffortIndicator (Interval e) tn =
        (ArithUpDn.MixedRingOpsEffortIndicator e tn,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator tn))
    mixedRingOpsDefaultEffort i@(Interval l r) n =
        (ArithUpDn.mixedRingOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n))
    mxringEffortAdd (Interval l r) n (effortRing, _) = 
        ArithUpDn.mxringEffortAdd l n effortRing
    mxringEffortMult (Interval l r) n (effortRing, (effortCompEpt, effortMinmax, effortCompS)) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxringEffortMult l n effortRing) 

instance (RoundedMixedAdd (Interval e) tn,
          RoundedMixedMultiply (Interval e) tn,
          RoundedMixedRingEffort (Interval e) tn) => 
        RoundedMixedRing (Interval e) tn
    
instance (RoundedMixedRingEffort (Interval e) tn,
          RoundedMixedDivideEffort (Interval e) tn,
          NumOrd.PartialComparison e,
          NumOrd.RoundedLatticeEffort e,
          NumOrd.PartialComparison tn,
          ArithUpDn.RoundedMixedFieldEffort e tn) => 
        RoundedMixedFieldEffort (Interval e) tn
    where
    type MixedFieldOpsEffortIndicator (Interval e) tn =
        (ArithUpDn.MixedFieldOpsEffortIndicator e tn,
         (NumOrd.PartialCompareEffortIndicator e, 
          NumOrd.MinmaxEffortIndicator e, 
          NumOrd.PartialCompareEffortIndicator tn),
         MixedDivEffortIndicator (Interval e) tn)
    mixedFieldOpsDefaultEffort i@(Interval l r) n =
        (ArithUpDn.mixedFieldOpsDefaultEffort l n,
         (NumOrd.pCompareDefaultEffort l,
          NumOrd.minmaxDefaultEffort l,
          NumOrd.pCompareDefaultEffort n),
         mixedDivDefaultEffort i n)
    mxfldEffortAdd (Interval l r) n (effortFld, _, _) = 
        ArithUpDn.mxfldEffortAdd l n effortFld
    mxfldEffortMult (Interval l r) n (effortFld, (effortCompEpt, effortMinmax, effortCompS), _) =
        ((effortCompS, effortCompEpt), 
          effortMinmax, 
          ArithUpDn.mxfldEffortMult l n effortFld) 
    mxfldEffortDiv _ _ (_, _, effortDiv) = effortDiv
    
instance (RoundedMixedRing (Interval e) tn,
          RoundedMixedDivide (Interval e) tn,
          RoundedMixedFieldEffort (Interval e) tn) => 
        RoundedMixedField (Interval e) tn



{- to allow the above instance to be applied when tn = Interval e,
   ie to allow "mixed" addition of Interval and Interval, 
   we define the following weird mixed operations: -}
   
instance (ArithUpDn.RoundedAddEffort e) => 
    ArithUpDn.RoundedMixedAddEffort e (Interval e)
    where
    type ArithUpDn.MixedAddEffortIndicator e (Interval e) = ArithUpDn.AddEffortIndicator e
    mixedAddDefaultEffort e (Interval l r) = 
        ArithUpDn.addDefaultEffort l
   
instance (ArithUpDn.RoundedAdd e) => 
    ArithUpDn.RoundedMixedAdd e (Interval e) 
    where
    mixedAddUpEff effort e i =
        case addOutEff effort (Interval e e) i of
            Interval l r -> r  
    mixedAddDnEff effort e i =
        case addOutEff effort (Interval e e) i of
            Interval l r -> l  

instance 
    (ArithUpDn.RoundedMultiplyEffort e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e
    ) 
    => 
    ArithUpDn.RoundedMixedMultiplyEffort e (Interval e) 
    where
    type ArithUpDn.MixedMultEffortIndicator e (Interval e) = 
        MultEffortIndicator (Interval e)
    mixedMultDefaultEffort e i = 
        multDefaultEffort i
   
instance 
    (ArithUpDn.RoundedMultiply e,
     HasZero e, Neg e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e
    )
    =>
    ArithUpDn.RoundedMixedMultiply e (Interval e) 
    where
    mixedMultUpEff effort e i =
        case multOutEff effort (Interval e e) i of
            Interval l r -> r  
    mixedMultDnEff effort e i =
        case multOutEff effort (Interval e e) i of
            Interval l r -> l  

instance 
    (ArithUpDn.RoundedMultiplyEffort e,
     ArithUpDn.RoundedDivideEffort e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e
    ) 
    => 
    ArithUpDn.RoundedMixedDivideEffort e (Interval e) 
    where
    type ArithUpDn.MixedDivEffortIndicator e (Interval e) = 
        DivEffortIndicator (Interval e)
    mixedDivDefaultEffort e i = 
        divDefaultEffort i
   
instance 
    (ArithUpDn.RoundedMultiply e,
     ArithUpDn.RoundedDivide e,
     NumOrd.HasExtrema e,
     HasZero e, Neg e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e
    )
    =>
    ArithUpDn.RoundedMixedDivide e (Interval e) 
    where
    mixedDivUpEff effort e i =
        case divOutEff effort (Interval e e) i of
            Interval l r -> r  
    mixedDivDnEff effort e i =
        case divOutEff effort (Interval e e) i of
            Interval l r -> l  

instance
    (ArithUpDn.RoundedRingEffort e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e
    )
    =>
    ArithUpDn.RoundedMixedRingEffort e (Interval e)
    where
    type ArithUpDn.MixedRingOpsEffortIndicator e (Interval e) =
        (RingOpsEffortIndicator (Interval e))
    mixedRingOpsDefaultEffort _ sampleI = ringOpsDefaultEffort sampleI
    mxringEffortAdd _ sampleI eff = ringEffortAdd sampleI eff
    mxringEffortMult _ sampleI eff = ringEffortMult sampleI eff

instance
    (ArithUpDn.RoundedRing e,
     NumOrd.HasExtrema e,
     HasZero e, Neg e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e
    )
    =>
    ArithUpDn.RoundedMixedRing e (Interval e)

instance
    (ArithUpDn.RoundedFieldEffort e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLatticeEffort e
    )
    =>
    ArithUpDn.RoundedMixedFieldEffort e (Interval e)
    where
    type ArithUpDn.MixedFieldOpsEffortIndicator e (Interval e) =
        (FieldOpsEffortIndicator (Interval e))
    mixedFieldOpsDefaultEffort _ sampleI = fieldOpsDefaultEffort sampleI
    mxfldEffortAdd _ sampleI eff = fldEffortAdd sampleI eff
    mxfldEffortMult _ sampleI eff = fldEffortMult sampleI eff
    mxfldEffortDiv _ sampleI eff = fldEffortDiv sampleI eff

instance
    (ArithUpDn.RoundedField e,
     NumOrd.HasExtrema e,
     HasZero e, Neg e,
     NumOrd.PartialComparison e,
     NumOrd.RoundedLattice e
    )
    =>
    ArithUpDn.RoundedMixedField e (Interval e)

{- end of weird mixed operations -}   

    