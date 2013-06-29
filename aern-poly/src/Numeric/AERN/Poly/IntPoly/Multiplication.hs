{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Multiplication
    Description :  out-rounded polynomial multiplication
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Out-rounded polynomial multiplication and scaling.
-}

module Numeric.AERN.Poly.IntPoly.Multiplication
    (
        multTerms,
        powTerms,
        scaleTerms
    )
where
    
import Prelude hiding ((+),(*))
import qualified Prelude
    
import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.New
import Numeric.AERN.Poly.IntPoly.Reduction
import Numeric.AERN.Poly.IntPoly.Addition

--import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain

--import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
--import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
    (MultEffortIndicator, PowerToNonnegIntEffortIndicator, RingOpsEffortIndicator, 
     MixedMultEffortIndicator, MixedRingOpsEffortIndicator, 
     MixedDivEffortIndicator, MixedFieldOpsEffortIndicator)
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.Auxiliary

import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsImplicitEffort
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

--import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap


instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedMultiplyEffort (IntPoly var cf) 
    where
    type MultEffortIndicator (IntPoly var cf) = 
        ArithInOut.RoundedRealEffortIndicator cf 
    multDefaultEffort (IntPoly cfg _) = 
        ArithInOut.roundedRealDefaultEffort (ipolycfg_sample_cf cfg)

instance
    (ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf, 
     Ord var, Show var, Show cf) 
    =>
    ArithInOut.RoundedMultiply (IntPoly var cf) 
    where
    multOutEff = multPolysOut 
    multInEff = 
        error "aern-poly: IntPoly does not support inwards-rounded multiplication" 

multPolysOut ::
    (Ord var, Show var, Show cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedReal cf, HasAntiConsistency cf) 
    =>
    (ArithInOut.RoundedRealEffortIndicator cf) -> 
    IntPoly var cf -> IntPoly var cf -> IntPoly var cf
multPolysOut eff p1@(IntPoly cfg terms1) (IntPoly _ terms2)
    =
    reducePolyTermCountOut eff $ 
        reducePolyDegreeOut eff $ 
            IntPoly cfg $
                termsNormalise $ 
                    let (<+>>) = ArithInOut.addOutEff effAdd in
                    let (<*>>) = ArithInOut.multOutEff effMult in
                    multTerms (<+>>) (<*>>) terms1 terms2
    where
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    sample = getSampleDomValue p1
    
multTerms ::
    (Show var, Show cf) 
    =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    IntPolyTerms var cf -> IntPolyTerms var cf -> IntPolyTerms var cf
multTerms _ (*) _terms1@(IntPolyC val1) _terms2@(IntPolyC val2) 
    = 
    IntPolyC $ val1 * val2 
multTerms (+) (*) _terms1@(IntPolyV _ powers1) _terms2@(IntPolyV xName2 powers2)
    =
    IntPolyV xName2 multSubPolys
    where
    multSubPolys 
        = IntMap.fromListWith (addTerms (+)) $
            [(n1 Prelude.+ n2, multTerms (+) (*) t1 t2) | 
                (n1, t1) <- IntMap.toAscList powers1, 
                (n2, t2) <- IntMap.toAscList powers2 ] 
multTerms _ _ terms1 terms2
    =
    error $ "AERN internal error: multTerms: incompatible operands: "
        ++ "\n terms1 = " ++ show terms1
        ++ "\n terms2 = " ++ show terms2


instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedPowerToNonnegIntEffort (IntPoly var cf)
    where
    type PowerToNonnegIntEffortIndicator (IntPoly var cf) =
         ArithInOut.PowerToNonnegIntEffortIndicatorFromMult (IntPoly var cf)
    powerToNonnegIntDefaultEffort = ArithInOut.powerToNonnegIntDefaultEffortFromMult

instance
    (ArithInOut.RoundedReal cf, 
     RefOrd.IntervalLike cf, 
     HasAntiConsistency cf,
--     NumOrd.PartialComparison (Imprecision cf), 
--     Show (Imprecision cf),
     Show var, Show cf, Ord var) 
    =>
    ArithInOut.RoundedPowerToNonnegInt (IntPoly var cf) 
    where
    powerToNonnegIntInEff eff p n =
        flipConsistencyPoly $ 
            ArithInOut.powerToNonnegIntOutEff eff (flipConsistencyPoly p) n
    powerToNonnegIntOutEff eff (IntPoly cfg terms) n = 
        IntPoly cfg $ 
            powTerms
                eff
                sample cfg
                (ArithInOut.addOutEff effAdd) 
                (ArithInOut.multOutEff effMult) 
                terms n
        where
        sample = ipolycfg_sample_cf cfg
        effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
--        effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
--        effImpr = ArithInOut.rrEffortImprecision sample eff
        
powTerms ::
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf, 
     HasAntiConsistency cf) 
    =>
    (ArithInOut.RoundedRealEffortIndicator cf) ->
    cf ->
    (IntPolyCfg var cf) ->
    (cf -> cf -> cf) ->
    (cf -> cf -> cf) ->
    IntPolyTerms var cf -> Int -> IntPolyTerms var cf
powTerms eff sample cfg (+) (*) = 
    powerFromMult 
        (mkConstTerms (one sample) vars) 
        multTermsReduce
        where
        vars = ipolycfg_vars cfg
        multTermsReduce t1 t2 =
            reduceTermsTermCountOut eff cfg $ 
                reduceTermsDegreeOut eff cfg $ 
                    termsNormalise $ multTerms (+) (*) t1 t2
    
    
instance
    (ArithInOut.RoundedMixedMultiplyEffort cf other) => 
    ArithInOut.RoundedMixedMultiplyEffort (IntPoly var cf) other 
    where
    type MixedMultEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedMultEffortIndicator cf other  
    mixedMultDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedMultDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedMultiply cf other,  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedMultiply (IntPoly var cf) other 
    where
    mixedMultOutEff eff (IntPoly cfg terms) a = 
        IntPoly cfg $ scaleTerms (*|) a terms
        where
        (*|) = ArithInOut.mixedMultOutEff eff
    mixedMultInEff =
        error "aern-poly: IntPoly does not support inwards-rounded mixed multiplication" 

scaleTerms :: 
    (cf -> t -> cf) -> 
    t -> 
    IntPolyTerms var cf -> 
    IntPolyTerms var cf
scaleTerms (*|) factor terms =
    termsMapCoeffs (*| factor) terms
        

instance
    (ArithInOut.RoundedReal cf) 
    => 
    ArithInOut.RoundedRingEffort (IntPoly var cf)
    where
    type RingOpsEffortIndicator (IntPoly var cf) =
        (ArithInOut.RoundedRealEffortIndicator cf)
    ringOpsDefaultEffort (IntPoly cfg _) = 
        ArithInOut.roundedRealDefaultEffort $ ipolycfg_sample_cf cfg
    ringEffortAdd _ eff = eff  
    ringEffortMult _ eff = eff
    ringEffortPow _ eff = eff  


instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRingEffort cf other) 
    => 
    ArithInOut.RoundedMixedRingEffort (IntPoly var cf) other
    where
    type MixedRingOpsEffortIndicator (IntPoly var cf) other =
        ArithInOut.MixedRingOpsEffortIndicator cf other
    mixedRingOpsDefaultEffort (IntPoly cfg _) other = 
        ArithInOut.mixedRingOpsDefaultEffort (ipolycfg_sample_cf cfg) other
    mxringEffortAdd  (IntPoly cfg _) eff = 
        ArithInOut.mxringEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxringEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxringEffortMult (ipolycfg_sample_cf cfg) eff  

instance 
    (ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedRing (IntPoly var cf)

instance 
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRing cf other,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedRing (IntPoly var cf) other


instance
    (ArithInOut.RoundedMixedDivideEffort cf other) => 
    ArithInOut.RoundedMixedDivideEffort (IntPoly var cf) other 
    where
    type MixedDivEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedDivEffortIndicator cf other  
    mixedDivDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedDivDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedDivide cf other,  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedDivide (IntPoly var cf) other 
    where
    mixedDivOutEff eff (IntPoly cfg terms) a = 
        IntPoly cfg $ scaleTerms (/|) a terms
        where
        (/|) = ArithInOut.mixedDivOutEff eff
    mixedDivInEff =
        error "aern-poly: IntPoly does not support inwards-rounded mixed division" 
        
instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedFieldEffort cf other) 
    => 
    ArithInOut.RoundedMixedFieldEffort (IntPoly var cf) other
    where
    type MixedFieldOpsEffortIndicator (IntPoly var cf) other =
        ArithInOut.MixedFieldOpsEffortIndicator cf other
    mixedFieldOpsDefaultEffort (IntPoly cfg _) other = 
        ArithInOut.mixedFieldOpsDefaultEffort (ipolycfg_sample_cf cfg) other
    mxfldEffortAdd  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxfldEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortMult (ipolycfg_sample_cf cfg) eff  
    mxfldEffortDiv  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortDiv (ipolycfg_sample_cf cfg) eff  

instance 
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedField cf other,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedField (IntPoly var cf) other
        
