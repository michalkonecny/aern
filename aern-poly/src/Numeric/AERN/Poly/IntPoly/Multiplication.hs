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

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
    (MultEffortIndicator, PowerToNonnegIntEffortIndicator, RingOpsEffortIndicator, 
     MixedMultEffortIndicator, MixedRingOpsEffortIndicator, 
     MixedDivEffortIndicator, MixedFieldOpsEffortIndicator)

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.Auxiliary

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Interval
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
    (ArithInOut.RoundedMixedMultiplyEffort cf Integer) => 
    ArithInOut.RoundedMixedMultiplyEffort (IntPoly var cf) Integer 
    where
    type MixedMultEffortIndicator (IntPoly var cf) Integer = 
        ArithInOut.MixedMultEffortIndicator cf Integer  
    mixedMultDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedMultDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedMultiply cf Integer,  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedMultiply (IntPoly var cf) Integer 
    where
    mixedMultOutEff = mixedMultOutEffGeneric
    mixedMultInEff = mixedMultInEffGeneric

instance
    (ArithInOut.RoundedMixedMultiplyEffort cf Int) => 
    ArithInOut.RoundedMixedMultiplyEffort (IntPoly var cf) Int 
    where
    type MixedMultEffortIndicator (IntPoly var cf) Int = 
        ArithInOut.MixedMultEffortIndicator cf Int  
    mixedMultDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedMultDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedMultiply cf Int,  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedMultiply (IntPoly var cf) Int 
    where
    mixedMultOutEff = mixedMultOutEffGeneric
    mixedMultInEff = mixedMultInEffGeneric

instance
    (ArithInOut.RoundedMixedMultiplyEffort cf Rational) => 
    ArithInOut.RoundedMixedMultiplyEffort (IntPoly var cf) Rational 
    where
    type MixedMultEffortIndicator (IntPoly var cf) Rational = 
        ArithInOut.MixedMultEffortIndicator cf Rational  
    mixedMultDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedMultDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedMultiply cf Rational,  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedMultiply (IntPoly var cf) Rational 
    where
    mixedMultOutEff = mixedMultOutEffGeneric
    mixedMultInEff = mixedMultInEffGeneric

instance
    (ArithInOut.RoundedMixedMultiplyEffort cf Double) => 
    ArithInOut.RoundedMixedMultiplyEffort (IntPoly var cf) Double 
    where
    type MixedMultEffortIndicator (IntPoly var cf) Double = 
        ArithInOut.MixedMultEffortIndicator cf Double  
    mixedMultDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedMultDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedMultiply cf Double,  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedMultiply (IntPoly var cf) Double 
    where
    mixedMultOutEff = mixedMultOutEffGeneric
    mixedMultInEff = mixedMultInEffGeneric


instance
    (ArithInOut.RoundedMixedMultiplyEffort cf (Interval e)) => 
    ArithInOut.RoundedMixedMultiplyEffort (IntPoly var cf) (Interval e) 
    where
    type MixedMultEffortIndicator (IntPoly var cf) (Interval e) = 
        ArithInOut.MixedMultEffortIndicator cf (Interval e)  
    mixedMultDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedMultDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedMultiply cf (Interval e),  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedMultiply (IntPoly var cf) (Interval e) 
    where
    mixedMultOutEff = mixedMultOutEffGeneric
    mixedMultInEff = mixedMultInEffGeneric



mixedMultOutEffGeneric, mixedMultInEffGeneric :: 
     ArithInOut.RoundedMixedMultiply cf t 
     =>
     MixedMultEffortIndicator cf t
     -> IntPoly var cf -> t -> IntPoly var cf
mixedMultOutEffGeneric eff (IntPoly cfg terms) a = 
    IntPoly cfg $ scaleTerms (*|) a terms
    where
    (*|) = ArithInOut.mixedMultOutEff eff
mixedMultInEffGeneric =
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
    ringEffortAdd _ eff = undefined -- TODO eff  
    ringEffortMult _ eff = eff
    ringEffortPow _ eff = eff  

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
     ArithInOut.RoundedMixedRingEffort cf Integer) 
    => 
    ArithInOut.RoundedMixedRingEffort (IntPoly var cf) Integer
    where
    type MixedRingOpsEffortIndicator (IntPoly var cf) Integer =
        ArithInOut.MixedRingOpsEffortIndicator cf Integer
    mixedRingOpsDefaultEffort (IntPoly cfg _) sample = 
        ArithInOut.mixedRingOpsDefaultEffort (ipolycfg_sample_cf cfg) sample
    mxringEffortAdd  (IntPoly cfg _) eff = 
        undefined -- TODO
--        ArithInOut.mxringEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxringEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxringEffortMult (ipolycfg_sample_cf cfg) eff  

instance 
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRing cf Integer,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedRing (IntPoly var cf) Integer

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRingEffort cf Int) 
    => 
    ArithInOut.RoundedMixedRingEffort (IntPoly var cf) Int
    where
    type MixedRingOpsEffortIndicator (IntPoly var cf) Int =
        ArithInOut.MixedRingOpsEffortIndicator cf Int
    mixedRingOpsDefaultEffort (IntPoly cfg _) sample = 
        ArithInOut.mixedRingOpsDefaultEffort (ipolycfg_sample_cf cfg) sample
    mxringEffortAdd  (IntPoly cfg _) eff = 
        undefined -- TODO
--        ArithInOut.mxringEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxringEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxringEffortMult (ipolycfg_sample_cf cfg) eff  

instance 
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRing cf Int,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedRing (IntPoly var cf) Int

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRingEffort cf Rational) 
    => 
    ArithInOut.RoundedMixedRingEffort (IntPoly var cf) Rational
    where
    type MixedRingOpsEffortIndicator (IntPoly var cf) Rational =
        ArithInOut.MixedRingOpsEffortIndicator cf Rational
    mixedRingOpsDefaultEffort (IntPoly cfg _) sample = 
        ArithInOut.mixedRingOpsDefaultEffort (ipolycfg_sample_cf cfg) sample
    mxringEffortAdd  (IntPoly cfg _) eff = 
        undefined -- TODO
--        ArithInOut.mxringEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxringEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxringEffortMult (ipolycfg_sample_cf cfg) eff  

instance 
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRing cf Rational,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedRing (IntPoly var cf) Rational

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRingEffort cf Double) 
    => 
    ArithInOut.RoundedMixedRingEffort (IntPoly var cf) Double
    where
    type MixedRingOpsEffortIndicator (IntPoly var cf) Double =
        ArithInOut.MixedRingOpsEffortIndicator cf Double
    mixedRingOpsDefaultEffort (IntPoly cfg _) sample = 
        ArithInOut.mixedRingOpsDefaultEffort (ipolycfg_sample_cf cfg) sample
    mxringEffortAdd  (IntPoly cfg _) eff = 
        undefined -- TODO
--        ArithInOut.mxringEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxringEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxringEffortMult (ipolycfg_sample_cf cfg) eff  

instance 
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRing cf Double,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedRing (IntPoly var cf) Double


instance
    (cf ~ Interval e,
     ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRingEffort cf (Interval e)) 
    => 
    ArithInOut.RoundedMixedRingEffort (IntPoly var cf) (Interval e)
    where
    type MixedRingOpsEffortIndicator (IntPoly var cf) (Interval e) =
        ArithInOut.MixedRingOpsEffortIndicator (Interval e) (Interval e)
    mixedRingOpsDefaultEffort (IntPoly cfg _) sample = 
        ArithInOut.mixedRingOpsDefaultEffort (ipolycfg_sample_cf cfg) sample
    mxringEffortAdd  (IntPoly cfg _) eff = 
        undefined -- TODO
--        ArithInOut.mxringEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxringEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxringEffortMult (ipolycfg_sample_cf cfg) eff  

instance 
    (cf ~ Interval e,
     ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedRing cf (Interval e),
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedRing (IntPoly var (Interval e)) (Interval e)



instance
    (ArithInOut.RoundedMixedDivideEffort cf Integer) => 
    ArithInOut.RoundedMixedDivideEffort (IntPoly var cf) Integer 
    where
    type MixedDivEffortIndicator (IntPoly var cf) Integer = 
        ArithInOut.MixedDivEffortIndicator cf Integer  
    mixedDivDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedDivDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedDivide cf Integer,  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedDivide (IntPoly var cf) Integer 
    where
    mixedDivOutEff = mixedDivOutEffGeneric
    mixedDivInEff = mixedDivInEffGeneric
    
instance
    (ArithInOut.RoundedMixedDivideEffort cf Int) => 
    ArithInOut.RoundedMixedDivideEffort (IntPoly var cf) Int 
    where
    type MixedDivEffortIndicator (IntPoly var cf) Int = 
        ArithInOut.MixedDivEffortIndicator cf Int  
    mixedDivDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedDivDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedDivide cf Int,  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedDivide (IntPoly var cf) Int 
    where
    mixedDivOutEff = mixedDivOutEffGeneric
    mixedDivInEff = mixedDivInEffGeneric
    
instance
    (ArithInOut.RoundedMixedDivideEffort cf Rational) => 
    ArithInOut.RoundedMixedDivideEffort (IntPoly var cf) Rational 
    where
    type MixedDivEffortIndicator (IntPoly var cf) Rational = 
        ArithInOut.MixedDivEffortIndicator cf Rational  
    mixedDivDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedDivDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedDivide cf Rational,  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedDivide (IntPoly var cf) Rational 
    where
    mixedDivOutEff = mixedDivOutEffGeneric
    mixedDivInEff = mixedDivInEffGeneric
    
instance
    (ArithInOut.RoundedMixedDivideEffort cf Double) => 
    ArithInOut.RoundedMixedDivideEffort (IntPoly var cf) Double 
    where
    type MixedDivEffortIndicator (IntPoly var cf) Double = 
        ArithInOut.MixedDivEffortIndicator cf Double  
    mixedDivDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedDivDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedDivide cf Double,  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedDivide (IntPoly var cf) Double 
    where
    mixedDivOutEff = mixedDivOutEffGeneric
    mixedDivInEff = mixedDivInEffGeneric

instance
    (ArithInOut.RoundedMixedDivideEffort cf (Interval e)) => 
    ArithInOut.RoundedMixedDivideEffort (IntPoly var cf) (Interval e) 
    where
    type MixedDivEffortIndicator (IntPoly var cf) (Interval e) = 
        ArithInOut.MixedDivEffortIndicator cf (Interval e)  
    mixedDivDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedDivDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedDivide cf (Interval e),  Ord var, 
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedDivide (IntPoly var cf) (Interval e) 
    where
    mixedDivOutEff = mixedDivOutEffGeneric
    mixedDivInEff = mixedDivInEffGeneric
    
mixedDivOutEffGeneric, mixedDivInEffGeneric :: 
    ArithInOut.RoundedMixedDivide cf t 
    =>
    MixedDivEffortIndicator cf t
    -> IntPoly var cf -> t -> IntPoly var cf
mixedDivOutEffGeneric eff (IntPoly cfg terms) a = 
    IntPoly cfg $ scaleTerms (/|) a terms
    where
    (/|) = ArithInOut.mixedDivOutEff eff
mixedDivInEffGeneric =
    error "aern-poly: IntPoly does not support inwards-rounded mixed division" 
        
        
instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedFieldEffort cf Integer) 
    => 
    ArithInOut.RoundedMixedFieldEffort (IntPoly var cf) Integer
    where
    type MixedFieldOpsEffortIndicator (IntPoly var cf) Integer =
        ArithInOut.MixedFieldOpsEffortIndicator cf Integer
    mixedFieldOpsDefaultEffort (IntPoly cfg _) sample = 
        ArithInOut.mixedFieldOpsDefaultEffort (ipolycfg_sample_cf cfg) sample
    mxfldEffortAdd  (IntPoly cfg _) eff = 
        undefined -- TODO
--        ArithInOut.mxfldEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxfldEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortMult (ipolycfg_sample_cf cfg) eff  
    mxfldEffortDiv  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortDiv (ipolycfg_sample_cf cfg) eff  

instance 
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedField cf Integer,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedField (IntPoly var cf) Integer
        
instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedFieldEffort cf Int) 
    => 
    ArithInOut.RoundedMixedFieldEffort (IntPoly var cf) Int
    where
    type MixedFieldOpsEffortIndicator (IntPoly var cf) Int =
        ArithInOut.MixedFieldOpsEffortIndicator cf Int
    mixedFieldOpsDefaultEffort (IntPoly cfg _) sample = 
        ArithInOut.mixedFieldOpsDefaultEffort (ipolycfg_sample_cf cfg) sample
    mxfldEffortAdd  (IntPoly cfg _) eff = 
        undefined -- TODO
--        ArithInOut.mxfldEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxfldEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortMult (ipolycfg_sample_cf cfg) eff  
    mxfldEffortDiv  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortDiv (ipolycfg_sample_cf cfg) eff  

instance 
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedField cf Int,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedField (IntPoly var cf) Int

instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedFieldEffort cf Rational) 
    => 
    ArithInOut.RoundedMixedFieldEffort (IntPoly var cf) Rational
    where
    type MixedFieldOpsEffortIndicator (IntPoly var cf) Rational =
        ArithInOut.MixedFieldOpsEffortIndicator cf Rational
    mixedFieldOpsDefaultEffort (IntPoly cfg _) sample = 
        ArithInOut.mixedFieldOpsDefaultEffort (ipolycfg_sample_cf cfg) sample
    mxfldEffortAdd  (IntPoly cfg _) eff = 
        undefined -- TODO
--        ArithInOut.mxfldEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxfldEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortMult (ipolycfg_sample_cf cfg) eff  
    mxfldEffortDiv  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortDiv (ipolycfg_sample_cf cfg) eff  

instance 
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedField cf Rational,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedField (IntPoly var cf) Rational
        
instance
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedFieldEffort cf Double) 
    => 
    ArithInOut.RoundedMixedFieldEffort (IntPoly var cf) Double
    where
    type MixedFieldOpsEffortIndicator (IntPoly var cf) Double =
        ArithInOut.MixedFieldOpsEffortIndicator cf Double
    mixedFieldOpsDefaultEffort (IntPoly cfg _) sample = 
        ArithInOut.mixedFieldOpsDefaultEffort (ipolycfg_sample_cf cfg) sample
    mxfldEffortAdd  (IntPoly cfg _) eff = 
        undefined -- TODO
--        ArithInOut.mxfldEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxfldEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortMult (ipolycfg_sample_cf cfg) eff  
    mxfldEffortDiv  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortDiv (ipolycfg_sample_cf cfg) eff  

instance 
    (ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedField cf Double,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedField (IntPoly var cf) Double

instance
    (cf ~ Interval e,
     ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedFieldEffort cf (Interval e)) 
    => 
    ArithInOut.RoundedMixedFieldEffort (IntPoly var (Interval e)) (Interval e)
    where
    type MixedFieldOpsEffortIndicator (IntPoly var (Interval e)) (Interval e) =
        ArithInOut.MixedFieldOpsEffortIndicator (Interval e) (Interval e)
    mixedFieldOpsDefaultEffort (IntPoly cfg _) sample = 
        ArithInOut.mixedFieldOpsDefaultEffort (ipolycfg_sample_cf cfg) sample
    mxfldEffortAdd  (IntPoly cfg _) eff = 
        undefined -- TODO
--        ArithInOut.mxfldEffortAdd (ipolycfg_sample_cf cfg) eff  
    mxfldEffortMult  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortMult (ipolycfg_sample_cf cfg) eff  
    mxfldEffortDiv  (IntPoly cfg _) eff = 
        ArithInOut.mxfldEffortDiv (ipolycfg_sample_cf cfg) eff  

instance 
    (cf ~ Interval e,
     ArithInOut.RoundedReal cf,
     ArithInOut.RoundedMixedField cf (Interval e),
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedMixedField (IntPoly var (Interval e)) (Interval e)

