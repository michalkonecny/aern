{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps
    Description :  refinement rounded ring operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Implementation of refinement rounded ring operations.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps
--    (
--    )
where

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.WithConsistentCoeffs
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps.Addition
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Reduce
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.WithConsistentCoeffs

import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.Auxiliary

import qualified Numeric.AERN.NumericOrder as NumOrd
import Numeric.AERN.NumericOrder.OpsImplicitEffort
import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap

instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedMultiplyEffort (IntPoly var cf) 
    where
    type ArithInOut.MultEffortIndicator (IntPoly var cf) = 
        (ArithInOut.RoundedRealEffortIndicator cf) 
    multDefaultEffort (IntPoly cfg _) = 
        (ArithInOut.roundedRealDefaultEffort sample_cf) 
        where
        sample_cf = (ipolycfg_sample_cf cfg)
    
instance
    (ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf, 
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf),
     Ord var, Show var, Show cf) =>
    ArithInOut.RoundedMultiply (IntPoly var cf) 
    where
    multInEff eff p1 p2 = 
        flipConsistencyPoly $ 
            ArithInOut.multOutEff eff (flipConsistencyPoly p1) (flipConsistencyPoly p2)
    multOutEff eff p1 p2 = 
        multPolysOut
            (ArithInOut.addOutEff effAdd) 
            (ArithInOut.multOutEff effMult) 
            (ArithInOut.powerToNonnegIntOutEff effPwr) 
            (imprecisionOfEff effImpr) 
            p1 p2
        where
        effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
        effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
        effImpr = ArithInOut.rrEffortImprecision sample eff
        sample = getSampleDomValue p1
        
    
multPolysOut ::
    (Show var, Show cf,
     ArithInOut.RoundedReal cf, HasAntiConsistency cf,
     NumOrd.PartialComparison imprecision) 
    =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    (cf -> imprecision) -> 
    IntPoly var cf -> IntPoly var cf -> IntPoly var cf
multPolysOut (+) (*) (^) getImpr (IntPoly cfg terms1) (IntPoly _ terms2)
    =
    IntPoly cfg $ 
        reduceTermsCount (+) (*) (^) getImpr varDoms maxSize $ 
            reduceTermsDegree (+) (*) (^) varDoms maxDeg $ 
                termsNormalise $ multTerms (+) (*) terms1 terms2
    where
    maxSize = ipolycfg_maxsize cfg
    maxDeg = ipolycfg_maxdeg cfg
    varDoms = ipolycfg_domsLZ cfg
    
multTerms ::
    (Show var, Show cf) 
    =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    IntPolyTerms var cf -> IntPolyTerms var cf -> IntPolyTerms var cf
multTerms (+) (*) terms1@(IntPolyC val1) terms2@(IntPolyC val2) 
    = 
    IntPolyC $ val1 * val2 
multTerms (+) (*) terms1@(IntPolyV xName1 powers1) terms2@(IntPolyV xName2 powers2)
    =
    IntPolyV xName2 multSubPolys
    where
    multSubPolys 
        = IntMap.fromListWith (addTerms (+)) $
            [(n1 Prelude.+ n2, multTerms (+) (*) t1 t2) | 
                (n1, t1) <- IntMap.toAscList powers1, 
                (n2, t2) <- IntMap.toAscList powers2 ] 
multTerms (+) (*) terms1 terms2
    =
    error $ "AERN internal error: multTerms: incompatible operands: "
        ++ "\n terms1 = " ++ show terms1
        ++ "\n terms2 = " ++ show terms2
        
instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedPowerToNonnegIntEffort (IntPoly var cf)
    where
    type (ArithInOut.PowerToNonnegIntEffortIndicator (IntPoly var cf)) =
         ArithInOut.PowerToNonnegIntEffortIndicatorFromMult (IntPoly var cf)
    powerToNonnegIntDefaultEffort = ArithInOut.powerToNonnegIntDefaultEffortFromMult

instance
    (ArithInOut.RoundedReal cf, HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf),
     Show var, Show cf, Ord var) =>
    ArithInOut.RoundedPowerToNonnegInt (IntPoly var cf) 
    where
    powerToNonnegIntInEff eff p n =
        flipConsistencyPoly $ 
            ArithInOut.powerToNonnegIntOutEff eff (flipConsistencyPoly p) n
    powerToNonnegIntOutEff eff (IntPoly cfg terms) n = 
        IntPoly cfg $ 
            powTerms
                True
                (ArithInOut.addOutEff effAdd) 
                (ArithInOut.multOutEff effMult) 
                (ArithInOut.powerToNonnegIntOutEff effPwr) 
                (imprecisionOfEff effImpr) 
                sample cfg
                terms n
        where
        sample = ipolycfg_sample_cf cfg
        effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
        effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
        effImpr = ArithInOut.rrEffortImprecision sample eff
        
powTerms ::
    (Show var, Show cf,
     ArithInOut.RoundedReal cf, HasAntiConsistency cf,
     NumOrd.PartialComparison imprecision) 
    =>
    Bool ->
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    (cf -> imprecision) ->
    cf ->
    (IntPolyCfg var cf) ->
    IntPolyTerms var cf -> Int -> IntPolyTerms var cf
powTerms isOut (+) (*) (^) getImpr sample cfg = 
    powerFromMult 
        (mkConstTerms (one sample) vars) 
        multTermsReduce
        where
        vars = ipolycfg_vars cfg
        multTermsReduce t1 t2 =
            reduceTermsCount (+) (*) (^) getImpr varDoms maxSize $ 
                reduceTermsDegree (+) (*) (^) varDoms maxDeg $ 
                    termsNormalise $ multTerms (+) (*) t1 t2
        maxSize = ipolycfg_maxsize cfg
        maxDeg = ipolycfg_maxdeg cfg
        varDoms
            | isOut = ipolycfg_domsLZ cfg
            | otherwise = map flipConsistency $ ipolycfg_domsLZ cfg

        
instance
    (ArithInOut.RoundedReal cf) 
    => 
    ArithInOut.RoundedRingEffort (IntPoly var cf)
    where
    type ArithInOut.RingOpsEffortIndicator (IntPoly var cf) =
        (ArithInOut.RoundedRealEffortIndicator cf)
    ringOpsDefaultEffort (IntPoly cfg _) = 
        ArithInOut.roundedRealDefaultEffort $ ipolycfg_sample_cf cfg
    ringEffortAdd (IntPoly cfg _) eff = eff  
    ringEffortMult (IntPoly cfg _) eff = eff
    ringEffortPow (IntPoly cfg _) eff = eff  

instance 
    (ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedRing (IntPoly var cf)

        
instance
    (ArithInOut.RoundedMixedDivideEffort cf other,  Ord var, ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedMixedDivideEffort (IntPoly var cf) other 
    where
    type ArithInOut.MixedDivEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedDivEffortIndicator cf other 
    mixedDivDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedDivDefaultEffort (ipolycfg_sample_cf cfg) c
    
-- TODO: mixed multiplication and division
-- must do it by conversion even for Int 
    
    
--instance
--    (ArithInOut.RoundedMixedDivide cf other,  Ord var, ArithInOut.RoundedReal cf) =>
--    ArithInOut.RoundedMixedDivide (IntPoly var cf) other 
--    where
--    mixedDivInEff eff (IntPoly cfg terms) a = 
--        IntPoly cfg $ scaleTerms (/|) a terms
--        where
--        (/|) = ArithInOut.mixedDivInEff eff
--    mixedDivOutEff eff (IntPoly cfg terms) a = 
--        IntPoly cfg $ scaleTerms (/|) a terms
--        where
--        (/|) = ArithInOut.mixedDivOutEff eff
--
--instance
--    (ArithInOut.RoundedReal cf, ArithInOut.RoundedMixedRingEffort cf other, Ord var) 
--    => 
--    ArithInOut.RoundedMixedRingEffort (IntPoly var cf) other
--    where
--    type ArithInOut.MixedRingOpsEffortIndicator (IntPoly var cf) other =
--        (ArithInOut.RoundedRealEffortIndicator cf,
--         ArithInOut.MixedRingOpsEffortIndicator cf other)
--    mixedRingOpsDefaultEffort (IntPoly cfg _) sampleOther = 
--        (ArithInOut.roundedRealDefaultEffort sampleCf,
--         ArithInOut.mixedRingOpsDefaultEffort sampleCf sampleOther)
--         where
--         sampleCf = ipolycfg_sample_cf cfg
--    mxringEffortAdd (IntPoly cfg _) sampleOther (effRR, effMF) =  
--        ArithInOut.mxringEffortAdd sampleCf sampleOther effMF
--        where
--        sampleCf = ipolycfg_sample_cf cfg 
--    mxringEffortMult (IntPoly cfg _) sampleOther (effRR, effMF) =  
--        ArithInOut.mxringEffortMult sampleCf sampleOther effMF
--        where
--        sampleCf = ipolycfg_sample_cf cfg 
--
--instance
--    (ArithInOut.RoundedReal cf, ArithInOut.RoundedMixedFieldEffort cf other, Ord var) 
--    => 
--    ArithInOut.RoundedMixedFieldEffort (IntPoly var cf) other
--    where
--    type ArithInOut.MixedFieldOpsEffortIndicator (IntPoly var cf) other =
--        (ArithInOut.RoundedRealEffortIndicator cf,
--         ArithInOut.MixedFieldOpsEffortIndicator cf other)
--    mixedFieldOpsDefaultEffort (IntPoly cfg _) sampleOther = 
--        (ArithInOut.roundedRealDefaultEffort sampleCf,
--         ArithInOut.mixedFieldOpsDefaultEffort sampleCf sampleOther)
--         where
--         sampleCf = ipolycfg_sample_cf cfg
--    mxfldEffortAdd (IntPoly cfg _) sampleOther (effRR, effMF) =  
--        ArithInOut.mxfldEffortAdd sampleCf sampleOther effMF
--        where
--        sampleCf = ipolycfg_sample_cf cfg 
--    mxfldEffortMult (IntPoly cfg _) sampleOther (effRR, effMF) =  
--        ArithInOut.mxfldEffortMult sampleCf sampleOther effMF
--        where
--        sampleCf = ipolycfg_sample_cf cfg 
--    mxfldEffortDiv (IntPoly cfg _) sampleOther (effRR, effMF) =  
--        ArithInOut.mxfldEffortDiv sampleCf sampleOther effMF
--        where
--        sampleCf = ipolycfg_sample_cf cfg 
--
--instance 
--    (Ord var,
--     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
--     ArithInOut.RoundedMixedRing cf other)
--    =>
--    ArithInOut.RoundedMixedRing (IntPoly var cf) other
--
--instance 
--    (Ord var,
--     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
--     ArithInOut.RoundedMixedField cf other)
--    =>
--    ArithInOut.RoundedMixedField (IntPoly var cf) other
    
