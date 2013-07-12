{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
    Module      :  Numeric.AERN.Poly.IntPoly.UpDnField
    Description :  up/dn-rounded polynomial field operations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Up/dn-rounded polynomial field operations.
-}

module Numeric.AERN.Poly.IntPoly.UpDnField
    ()
where
    
import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
--import Numeric.AERN.Poly.IntPoly.Reduction
import Numeric.AERN.Poly.IntPoly.Conversion ()
import Numeric.AERN.Poly.IntPoly.Addition ()
import Numeric.AERN.Poly.IntPoly.Multiplication ()
import Numeric.AERN.Poly.IntPoly.Division ()

import Numeric.AERN.RmToRn

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.NumericOrderRounding
    (AddEffortIndicator, 
     MultEffortIndicator,
     PowerNonnegToNonnegIntEffortIndicator, 
     PowerToNonnegIntEffortIndicator,
     RingOpsEffortIndicator, 
     MixedAddEffortIndicator,
     MixedMultEffortIndicator,
     MixedRingOpsEffortIndicator,
     MixedDivEffortIndicator,
     MixedFieldOpsEffortIndicator
     )
--import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

--import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
--import Numeric.AERN.RealArithmetic.Auxiliary

--import Numeric.AERN.RealArithmetic.Interval

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsImplicitEffort
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Interval
import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Consistency

--import Numeric.AERN.Misc.Debug

--import qualified Data.IntMap as IntMap

{----- addition up/dn via out -----}    

instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedAddEffort (IntPoly var cf) 
    where
    type AddEffortIndicator (IntPoly var cf) = 
        (ArithInOut.AddEffortIndicator (IntPoly var cf), 
         RefOrd.GetEndpointsEffortIndicator cf) 
    addDefaultEffort p@(IntPoly cfg _) = 
        (ArithInOut.addDefaultEffort p,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedAdd (IntPoly var cf) 
    where
    addUpEff (effOut, effGetE) p1 p2 =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.addOutEff effOut p1 p2
    addDnEff (effOut, effGetE) p1 p2 =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.addOutEff effOut p1 p2

instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedSubtr (IntPoly var cf) 
    
  
{----- multiplication up/dn via out -----}    

instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMultiplyEffort (IntPoly var cf) 
    where
    type MultEffortIndicator (IntPoly var cf) = 
        (ArithInOut.MultEffortIndicator (IntPoly var cf), 
         RefOrd.GetEndpointsEffortIndicator cf) 
    multDefaultEffort p@(IntPoly cfg _) = 
        (ArithInOut.addDefaultEffort p,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMultiply (IntPoly var cf) 
    where
    multUpEff (effOut, effGetE) p1 p2 =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.multOutEff effOut p1 p2
    multDnEff (effOut, effGetE) p1 p2 =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.multOutEff effOut p1 p2
  
{----- integer power (both nonneg and general versions) up/dn via out -----}      

instance
    (Ord var,
     ArithInOut.RoundedReal cf, 
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedPowerNonnegToNonnegIntEffort (IntPoly var cf)
    where
    type PowerNonnegToNonnegIntEffortIndicator (IntPoly var cf) =
         (ArithInOut.PowerToNonnegIntEffortIndicatorFromMult (IntPoly var cf),
          RefOrd.GetEndpointsEffortIndicator cf)
    powerNonnegToNonnegIntDefaultEffort sampleP = 
        (ArithInOut.powerToNonnegIntDefaultEffortFromMult sampleP,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal cf, 
     RefOrd.IntervalLike cf, 
     HasAntiConsistency cf,
--     NumOrd.PartialComparison (Imprecision cf), 
--     Show (Imprecision cf),
     Show var, Show cf, Ord var) 
    =>
    ArithUpDn.RoundedPowerNonnegToNonnegInt (IntPoly var cf) 
    where
    powerNonnegToNonnegIntUpEff (effPow, effGetE) p n =
        snd $ RefOrd.getEndpointsOutEff effGetE $
            ArithInOut.powerToNonnegIntOutEff effPow p n   
    powerNonnegToNonnegIntDnEff (effPow, effGetE) p n =
        fst $ RefOrd.getEndpointsOutEff effGetE $
            ArithInOut.powerToNonnegIntOutEff effPow p n   
    
instance
    (Ord var,
     ArithInOut.RoundedReal cf, 
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedPowerToNonnegIntEffort (IntPoly var cf)
    where
    type PowerToNonnegIntEffortIndicator (IntPoly var cf) =
         (ArithInOut.PowerToNonnegIntEffortIndicatorFromMult (IntPoly var cf),
          RefOrd.GetEndpointsEffortIndicator cf)
    powerToNonnegIntDefaultEffort sampleP = 
        (ArithInOut.powerToNonnegIntDefaultEffortFromMult sampleP,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal cf, 
     RefOrd.IntervalLike cf, 
     HasAntiConsistency cf,
--     NumOrd.PartialComparison (Imprecision cf), 
--     Show (Imprecision cf),
     Show var, Show cf, Ord var) 
    =>
    ArithUpDn.RoundedPowerToNonnegInt (IntPoly var cf) 
    where
    powerToNonnegIntUpEff (effPow, effGetE) p n =
        snd $ RefOrd.getEndpointsOutEff effGetE $
            ArithInOut.powerToNonnegIntOutEff effPow p n   
    powerToNonnegIntDnEff (effPow, effGetE) p n =
        fst $ RefOrd.getEndpointsOutEff effGetE $
            ArithInOut.powerToNonnegIntOutEff effPow p n   
  
instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedRingEffort (IntPoly var cf)
    where
    type RingOpsEffortIndicator (IntPoly var cf) =
        (ArithInOut.RoundedRealEffortIndicator cf,
         RefOrd.GetEndpointsEffortIndicator cf)
    ringOpsDefaultEffort sampleP = 
        (ArithInOut.roundedRealDefaultEffort sampleCf,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    ringEffortAdd _ eff = eff  
    ringEffortMult _ eff = eff
    ringEffortPow _ eff = eff
  
instance 
    (ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithUpDn.RoundedRing (IntPoly var cf)


{----- up/dn division -----}    

instance
    (Ord var, Show var, Show cf, HasAntiConsistency cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedDivideEffort (IntPoly var cf) 
    where
    type DivEffortIndicator (IntPoly var cf) =
        (ArithInOut.DivEffortIndicator (IntPoly var cf), 
         RefOrd.GetEndpointsEffortIndicator cf) 
    divDefaultEffort p@(IntPoly cfg _) = 
        (ArithInOut.divDefaultEffort p,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedDivide (IntPoly var cf) 
    where
    divUpEff (effOut, effGetE) p1 p2 =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.divOutEff effOut p1 p2
    divDnEff (effOut, effGetE) p1 p2 =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.divOutEff effOut p1 p2
  

instance
    (Ord var, Show var, Show cf, HasAntiConsistency cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedFieldEffort (IntPoly var cf)
    where
    type FieldOpsEffortIndicator (IntPoly var cf) =
        ((ArithInOut.RoundedRealEffortIndicator cf, Int1To10),
         RefOrd.GetEndpointsEffortIndicator cf)
    fieldOpsDefaultEffort sampleP = 
        (evaluationDefaultEffort sampleP,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    fldEffortAdd _ ((effD, _), effGE) = (effD, effGE)  
    fldEffortMult _ ((effD, _), effGE) = (effD, effGE)
    fldEffortPow _ ((effD, _), effGE) = (effD, effGE)
    fldEffortDiv _ (effEval@(effD, _), effGE) = ((effEval, effD), effGE)
  
instance 
    (ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithUpDn.RoundedField (IntPoly var cf)

{----- mixed addition up/dn via out -----}    

instance
    (ArithInOut.RoundedMixedAddEffort (Interval e) (Interval e)) 
    =>
    ArithUpDn.RoundedMixedAddEffort (IntPoly var (Interval e)) (Interval e) 
    where
    type MixedAddEffortIndicator (IntPoly var (Interval e)) (Interval e) = 
        (ArithInOut.MixedAddEffortIndicator (IntPoly var (Interval e)) (Interval e), 
         RefOrd.GetEndpointsEffortIndicator (Interval e)) 
    mixedAddDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedAddDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedAddEffort cf Int,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedAddEffort (IntPoly var cf) Int 
    where
    type MixedAddEffortIndicator (IntPoly var cf) Int = 
        (ArithInOut.MixedAddEffortIndicator (IntPoly var cf) Int, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedAddDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedAddDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedAddEffort cf Integer,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedAddEffort (IntPoly var cf) Integer
    where
    type MixedAddEffortIndicator (IntPoly var cf) Integer = 
        (ArithInOut.MixedAddEffortIndicator (IntPoly var cf) Integer, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedAddDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedAddDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedAddEffort cf Rational,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedAddEffort (IntPoly var cf) Rational
    where
    type MixedAddEffortIndicator (IntPoly var cf) Rational = 
        (ArithInOut.MixedAddEffortIndicator (IntPoly var cf) Rational, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedAddDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedAddDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg


instance
    (ArithInOut.RoundedMixedAddEffort cf Double,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedAddEffort (IntPoly var cf) Double
    where
    type MixedAddEffortIndicator (IntPoly var cf) Double = 
        (ArithInOut.MixedAddEffortIndicator (IntPoly var cf) Double, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedAddDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedAddDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedAdd (Interval e) (Interval e),
     ArithInOut.RoundedReal (Interval e),
     RefOrd.IntervalLike (Interval e),  
     HasAntiConsistency (Interval e),
     Ord var, 
     Show var, Show (Interval e)) 
    =>
    ArithUpDn.RoundedMixedAdd (IntPoly var (Interval e)) (Interval e) 
    where
    mixedAddUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedAddOutEff effOut p1 other
    mixedAddDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedAddOutEff effOut p1 other
    
instance
    (ArithInOut.RoundedMixedAdd cf Int,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedAdd (IntPoly var cf) Int 
    where
    mixedAddUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedAddOutEff effOut p1 other
    mixedAddDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedAddOutEff effOut p1 other
    
instance
    (ArithInOut.RoundedMixedAdd cf Integer,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedAdd (IntPoly var cf) Integer 
    where
    mixedAddUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedAddOutEff effOut p1 other
    mixedAddDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedAddOutEff effOut p1 other
    
instance
    (ArithInOut.RoundedMixedAdd cf Rational,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedAdd (IntPoly var cf) Rational 
    where
    mixedAddUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedAddOutEff effOut p1 other
    mixedAddDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedAddOutEff effOut p1 other
    
instance
    (ArithInOut.RoundedMixedAdd cf Double,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedAdd (IntPoly var cf) Double 
    where
    mixedAddUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedAddOutEff effOut p1 other
    mixedAddDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedAddOutEff effOut p1 other



{----- mixed multiplication up/dn via out -----}    

instance
    (ArithInOut.RoundedMixedMultiplyEffort (Interval e) (Interval e)) 
    => 
    ArithUpDn.RoundedMixedMultiplyEffort (IntPoly var (Interval e)) (Interval e) 
    where
    type MixedMultEffortIndicator (IntPoly var (Interval e)) (Interval e) = 
        (ArithInOut.MixedMultEffortIndicator (IntPoly var (Interval e)) (Interval e), 
         RefOrd.GetEndpointsEffortIndicator (Interval e)) 
    mixedMultDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedMultDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedMultiplyEffort cf Int,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedMultiplyEffort (IntPoly var cf) Int 
    where
    type MixedMultEffortIndicator (IntPoly var cf) Int = 
        (ArithInOut.MixedMultEffortIndicator (IntPoly var cf) Int, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedMultDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedMultDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedMultiplyEffort cf Integer,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedMultiplyEffort (IntPoly var cf) Integer 
    where
    type MixedMultEffortIndicator (IntPoly var cf) Integer = 
        (ArithInOut.MixedMultEffortIndicator (IntPoly var cf) Integer, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedMultDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedMultDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedMultiplyEffort cf Double,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedMultiplyEffort (IntPoly var cf) Double 
    where
    type MixedMultEffortIndicator (IntPoly var cf) Double = 
        (ArithInOut.MixedMultEffortIndicator (IntPoly var cf) Double, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedMultDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedMultDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedMultiplyEffort cf Rational,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedMultiplyEffort (IntPoly var cf) Rational 
    where
    type MixedMultEffortIndicator (IntPoly var cf) Rational = 
        (ArithInOut.MixedMultEffortIndicator (IntPoly var cf) Rational, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedMultDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedMultDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedMultiply (Interval e) (Interval e),
     ArithInOut.RoundedReal (Interval e),
     RefOrd.IntervalLike (Interval e),  
     HasAntiConsistency (Interval e),
     Ord var, 
     Show var, Show (Interval e)) 
    =>
    ArithUpDn.RoundedMixedMultiply (IntPoly var (Interval e)) (Interval e) 
    where
    mixedMultUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedMultOutEff effOut p1 other
    mixedMultDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedMultOutEff effOut p1 other
    
instance
    (ArithInOut.RoundedMixedMultiply cf Int,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedMultiply (IntPoly var cf) Int 
    where
    mixedMultUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedMultOutEff effOut p1 other
    mixedMultDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedMultOutEff effOut p1 other
    
instance
    (ArithInOut.RoundedMixedMultiply cf Integer,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedMultiply (IntPoly var cf) Integer 
    where
    mixedMultUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedMultOutEff effOut p1 other
    mixedMultDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedMultOutEff effOut p1 other
    
instance
    (ArithInOut.RoundedMixedMultiply cf Double,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedMultiply (IntPoly var cf) Double 
    where
    mixedMultUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedMultOutEff effOut p1 other
    mixedMultDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedMultOutEff effOut p1 other
    
instance
    (ArithInOut.RoundedMixedMultiply cf Rational,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedMultiply (IntPoly var cf) Rational 
    where
    mixedMultUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedMultOutEff effOut p1 other
    mixedMultDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedMultOutEff effOut p1 other


instance
    (Ord var,
     ArithInOut.RoundedReal (Interval e),
     RefOrd.IntervalLike (Interval e),
     ArithInOut.RoundedMixedRingEffort (Interval e) (Interval e)) 
    => 
    ArithUpDn.RoundedMixedRingEffort (IntPoly var (Interval e)) (Interval e)
    where
    type MixedRingOpsEffortIndicator (IntPoly var (Interval e)) (Interval e) =
        (ArithInOut.MixedRingOpsEffortIndicator (Interval e) (Interval e),
         RefOrd.GetEndpointsEffortIndicator (Interval e))
    mixedRingOpsDefaultEffort sampleP other = 
        (ArithInOut.mixedRingOpsDefaultEffort sampleCf other,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    mxringEffortAdd sampleP sampleI (effRing, effGetE) = 
        (ArithInOut.mxringEffortAdd sampleCf sampleI effRing, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxringEffortMult sampleP sampleI (effRing, effGetE) = 
        (ArithInOut.mxringEffortMult sampleCf sampleI effRing, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal (Interval e),
     ArithInOut.RoundedMixedRing (Interval e) (Interval e),
     HasAntiConsistency (Interval e),
     RefOrd.IntervalLike (Interval e),
     Show var, Ord var, Show (Interval e),
     NumOrd.PartialComparison (Imprecision (Interval e)), Show (Imprecision (Interval e)))
    =>
    ArithUpDn.RoundedMixedRing (IntPoly var (Interval e)) (Interval e)

instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedRingEffort cf Int) 
    => 
    ArithUpDn.RoundedMixedRingEffort (IntPoly var cf) Int
    where
    type MixedRingOpsEffortIndicator (IntPoly var cf) Int =
        (ArithInOut.MixedRingOpsEffortIndicator cf Int,
         RefOrd.GetEndpointsEffortIndicator cf)
    mixedRingOpsDefaultEffort sampleP other = 
        (ArithInOut.mixedRingOpsDefaultEffort sampleCf other,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    mxringEffortAdd sampleP sampleI (effRing, effGetE) = 
        (ArithInOut.mxringEffortAdd sampleCf sampleI effRing, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxringEffortMult sampleP sampleI (effRing, effGetE) = 
        (ArithInOut.mxringEffortMult sampleCf sampleI effRing, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRing cf Int,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithUpDn.RoundedMixedRing (IntPoly var cf) Int

instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedRingEffort cf Integer) 
    => 
    ArithUpDn.RoundedMixedRingEffort (IntPoly var cf) Integer
    where
    type MixedRingOpsEffortIndicator (IntPoly var cf) Integer =
        (ArithInOut.MixedRingOpsEffortIndicator cf Integer,
         RefOrd.GetEndpointsEffortIndicator cf)
    mixedRingOpsDefaultEffort sampleP other = 
        (ArithInOut.mixedRingOpsDefaultEffort sampleCf other,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    mxringEffortAdd sampleP sampleI (effRing, effGetE) = 
        (ArithInOut.mxringEffortAdd sampleCf sampleI effRing, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxringEffortMult sampleP sampleI (effRing, effGetE) = 
        (ArithInOut.mxringEffortMult sampleCf sampleI effRing, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRing cf Integer,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithUpDn.RoundedMixedRing (IntPoly var cf) Integer

instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedRingEffort cf Rational) 
    => 
    ArithUpDn.RoundedMixedRingEffort (IntPoly var cf) Rational
    where
    type MixedRingOpsEffortIndicator (IntPoly var cf) Rational =
        (ArithInOut.MixedRingOpsEffortIndicator cf Rational,
         RefOrd.GetEndpointsEffortIndicator cf)
    mixedRingOpsDefaultEffort sampleP other = 
        (ArithInOut.mixedRingOpsDefaultEffort sampleCf other,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    mxringEffortAdd sampleP sampleI (effRing, effGetE) = 
        (ArithInOut.mxringEffortAdd sampleCf sampleI effRing, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxringEffortMult sampleP sampleI (effRing, effGetE) = 
        (ArithInOut.mxringEffortMult sampleCf sampleI effRing, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRing cf Rational,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithUpDn.RoundedMixedRing (IntPoly var cf) Rational
        
        

instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedRingEffort cf Double) 
    => 
    ArithUpDn.RoundedMixedRingEffort (IntPoly var cf) Double
    where
    type MixedRingOpsEffortIndicator (IntPoly var cf) Double =
        (ArithInOut.MixedRingOpsEffortIndicator cf Double,
         RefOrd.GetEndpointsEffortIndicator cf)
    mixedRingOpsDefaultEffort sampleP other = 
        (ArithInOut.mixedRingOpsDefaultEffort sampleCf other,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    mxringEffortAdd sampleP sampleI (effRing, effGetE) = 
        (ArithInOut.mxringEffortAdd sampleCf sampleI effRing, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxringEffortMult sampleP sampleI (effRing, effGetE) = 
        (ArithInOut.mxringEffortMult sampleCf sampleI effRing, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRing cf Double,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithUpDn.RoundedMixedRing (IntPoly var cf) Double

{----- mixed multiplication up/dn via out -----}    

instance
    (ArithInOut.RoundedMixedDivideEffort (Interval e) (Interval e)) 
    => 
    ArithUpDn.RoundedMixedDivideEffort (IntPoly var (Interval e)) (Interval e) 
    where
    type MixedDivEffortIndicator (IntPoly var (Interval e)) (Interval e) = 
        (ArithInOut.MixedDivEffortIndicator (IntPoly var (Interval e)) (Interval e), 
         RefOrd.GetEndpointsEffortIndicator (Interval e)) 
    mixedDivDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedDivDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedDivide (Interval e) (Interval e),
     ArithInOut.RoundedReal (Interval e),
     HasAntiConsistency (Interval e),
     Ord var, 
     Show var, Show (Interval e)) 
    =>
    ArithUpDn.RoundedMixedDivide (IntPoly var (Interval e)) (Interval e) 
    where
    mixedDivUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedDivOutEff effOut p1 other
    mixedDivDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedDivOutEff effOut p1 other

instance
    (ArithInOut.RoundedMixedDivideEffort cf Int,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedDivideEffort (IntPoly var cf) Int 
    where
    type MixedDivEffortIndicator (IntPoly var cf) Int = 
        (ArithInOut.MixedDivEffortIndicator (IntPoly var cf) Int, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedDivDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedDivDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedDivide cf Int,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedDivide (IntPoly var cf) Int 
    where
    mixedDivUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedDivOutEff effOut p1 other
    mixedDivDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedDivOutEff effOut p1 other

instance
    (ArithInOut.RoundedMixedDivideEffort cf Integer,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedDivideEffort (IntPoly var cf) Integer 
    where
    type MixedDivEffortIndicator (IntPoly var cf) Integer = 
        (ArithInOut.MixedDivEffortIndicator (IntPoly var cf) Integer, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedDivDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedDivDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedDivide cf Integer,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedDivide (IntPoly var cf) Integer 
    where
    mixedDivUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedDivOutEff effOut p1 other
    mixedDivDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedDivOutEff effOut p1 other

instance
    (ArithInOut.RoundedMixedDivideEffort cf Rational,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedDivideEffort (IntPoly var cf) Rational 
    where
    type MixedDivEffortIndicator (IntPoly var cf) Rational = 
        (ArithInOut.MixedDivEffortIndicator (IntPoly var cf) Rational, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedDivDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedDivDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedDivide cf Rational,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedDivide (IntPoly var cf) Rational 
    where
    mixedDivUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedDivOutEff effOut p1 other
    mixedDivDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedDivOutEff effOut p1 other

instance
    (ArithInOut.RoundedMixedDivideEffort cf Double,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedDivideEffort (IntPoly var cf) Double 
    where
    type MixedDivEffortIndicator (IntPoly var cf) Double = 
        (ArithInOut.MixedDivEffortIndicator (IntPoly var cf) Double, 
         RefOrd.GetEndpointsEffortIndicator cf) 
    mixedDivDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedDivDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedMixedDivide cf Double,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithUpDn.RoundedMixedDivide (IntPoly var cf) Double 
    where
    mixedDivUpEff (effOut, effGetE) p1 other =
        snd $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedDivOutEff effOut p1 other
    mixedDivDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOutEff effGetE $ ArithInOut.mixedDivOutEff effOut p1 other


instance
    (Ord var,
     ArithInOut.RoundedReal (Interval e),
     ArithInOut.RoundedMixedFieldEffort (Interval e) (Interval e)) 
    => 
    ArithUpDn.RoundedMixedFieldEffort (IntPoly var (Interval e)) (Interval e)
    where
    type MixedFieldOpsEffortIndicator (IntPoly var (Interval e)) (Interval e) =
        (ArithInOut.MixedFieldOpsEffortIndicator (Interval e) (Interval e),
         RefOrd.GetEndpointsEffortIndicator (Interval e))
    mixedFieldOpsDefaultEffort sampleP other = 
        (ArithInOut.mixedFieldOpsDefaultEffort sampleCf other,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortAdd sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortAdd sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortMult sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortMult sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortDiv sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortDiv sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal (Interval e),
     ArithInOut.RoundedMixedField (Interval e) (Interval e),
     HasAntiConsistency (Interval e),
     Show var, Ord var, Show (Interval e),
     NumOrd.PartialComparison (Imprecision (Interval e)), Show (Imprecision (Interval e)))
    =>
    ArithUpDn.RoundedMixedField (IntPoly var (Interval e)) (Interval e)

instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedFieldEffort cf Int) 
    => 
    ArithUpDn.RoundedMixedFieldEffort (IntPoly var cf) Int
    where
    type MixedFieldOpsEffortIndicator (IntPoly var cf) Int =
        (ArithInOut.MixedFieldOpsEffortIndicator cf Int,
         RefOrd.GetEndpointsEffortIndicator cf)
    mixedFieldOpsDefaultEffort sampleP other = 
        (ArithInOut.mixedFieldOpsDefaultEffort sampleCf other,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortAdd sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortAdd sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortMult sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortMult sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortDiv sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortDiv sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedField cf Int,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithUpDn.RoundedMixedField (IntPoly var cf) Int

instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedFieldEffort cf Integer) 
    => 
    ArithUpDn.RoundedMixedFieldEffort (IntPoly var cf) Integer
    where
    type MixedFieldOpsEffortIndicator (IntPoly var cf) Integer =
        (ArithInOut.MixedFieldOpsEffortIndicator cf Integer,
         RefOrd.GetEndpointsEffortIndicator cf)
    mixedFieldOpsDefaultEffort sampleP other = 
        (ArithInOut.mixedFieldOpsDefaultEffort sampleCf other,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortAdd sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortAdd sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortMult sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortMult sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortDiv sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortDiv sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedField cf Integer,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithUpDn.RoundedMixedField (IntPoly var cf) Integer

instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedFieldEffort cf Rational) 
    => 
    ArithUpDn.RoundedMixedFieldEffort (IntPoly var cf) Rational
    where
    type MixedFieldOpsEffortIndicator (IntPoly var cf) Rational =
        (ArithInOut.MixedFieldOpsEffortIndicator cf Rational,
         RefOrd.GetEndpointsEffortIndicator cf)
    mixedFieldOpsDefaultEffort sampleP other = 
        (ArithInOut.mixedFieldOpsDefaultEffort sampleCf other,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortAdd sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortAdd sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortMult sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortMult sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortDiv sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortDiv sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedField cf Rational,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithUpDn.RoundedMixedField (IntPoly var cf) Rational
        
instance
    (Ord var,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedFieldEffort cf Double) 
    => 
    ArithUpDn.RoundedMixedFieldEffort (IntPoly var cf) Double
    where
    type MixedFieldOpsEffortIndicator (IntPoly var cf) Double =
        (ArithInOut.MixedFieldOpsEffortIndicator cf Double,
         RefOrd.GetEndpointsEffortIndicator cf)
    mixedFieldOpsDefaultEffort sampleP other = 
        (ArithInOut.mixedFieldOpsDefaultEffort sampleCf other,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortAdd sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortAdd sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortMult sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortMult sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP
    mxfldEffortDiv sampleP sampleI (effField, effGetE) = 
        (ArithInOut.mxfldEffortDiv sampleCf sampleI effField, effGetE)  
        where
        sampleCf = getSampleDomValue sampleP

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedField cf Double,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithUpDn.RoundedMixedField (IntPoly var cf) Double
        
        
