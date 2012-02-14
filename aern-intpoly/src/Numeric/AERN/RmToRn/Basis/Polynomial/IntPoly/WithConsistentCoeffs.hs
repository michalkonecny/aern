{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.WithConsistentCoeffs
    Description :  operations assuming all coefficients are consistent 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Operations assuming all coefficients are consistent.
    Useful for implementing the Berstein approximation of
    min and max that cannot depend on the fully general
    multiplication as the multiplication requires min and max.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.WithConsistentCoeffs
--    (
--    )
where

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps.Addition
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps.TermWise
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Reduce

import Numeric.AERN.RmToRn.NumericOrder.FromInOutRingOps.Minmax

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

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Consistency

import Numeric.AERN.Misc.Debug

import Test.QuickCheck (Arbitrary)

import qualified Data.IntMap as IntMap


{-|
    A clone of the 'IntPoly' type that differs from 'IntPoly'
    in that its features only outwards rounded operations
    and these work correctly only on the assumption that all
    coefficients are consistent.  The main purpose of this
    type is to provide a means to perform min/max computation
    on the endpoint polynomials when computing 'IntPoly' multiplication.
    Thus one breaks the dependency cycle caused by the fact that
    IntPoly min/max operations require multiplication and multiplication
    requires min/max operations.  
-}
newtype IntPolyWithConsistentCoeffs var cf =
    IntPolyWithConsistentCoeffs (IntPoly var cf)
    deriving (Show)

instance
    (Ord var, ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf)
    =>
    HasDomainBox (IntPolyWithConsistentCoeffs var cf)
    where
    type Domain (IntPolyWithConsistentCoeffs var cf) = Domain (IntPoly var cf)
    type Var (IntPolyWithConsistentCoeffs var cf) = Var (IntPoly var cf)
    type VarBox (IntPolyWithConsistentCoeffs var cf) = VarBox (IntPoly var cf)
    getSampleDomValue (IntPolyWithConsistentCoeffs p) = getSampleDomValue p
    defaultDomSplit (IntPolyWithConsistentCoeffs p) = defaultDomSplit p
    getDomainBox (IntPolyWithConsistentCoeffs p) = getDomainBox p
    getNSamplesFromDomainBox (IntPolyWithConsistentCoeffs p) = getNSamplesFromDomainBox p
    getSampleFromInsideDomainBox (IntPolyWithConsistentCoeffs p) = getSampleFromInsideDomainBox p

instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedAddEffort (IntPolyWithConsistentCoeffs var cf) 
    where
    type ArithInOut.AddEffortIndicator (IntPolyWithConsistentCoeffs var cf) = 
        ArithInOut.AddEffortIndicator (IntPoly var cf) 
    addDefaultEffort (IntPolyWithConsistentCoeffs p) = 
        ArithInOut.addDefaultEffort p

instance
    (ArithInOut.RoundedReal cf,  HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf), 
     Show var, Show cf) 
    =>
    ArithInOut.RoundedAdd (IntPolyWithConsistentCoeffs var cf) 
    where
    addOutEff eff (IntPolyWithConsistentCoeffs p1) (IntPolyWithConsistentCoeffs p2) =
        IntPolyWithConsistentCoeffs $ ArithInOut.addOutEff eff p1 p2
    addInEff _ _ _ = 
        error $ "IntPolyWithConsistentCoeffs does not support inner rounded addition"

instance
    (Neg cf) => Neg (IntPolyWithConsistentCoeffs var cf)
    where
    neg (IntPolyWithConsistentCoeffs p) = IntPolyWithConsistentCoeffs $ negPoly p

instance
    (ArithInOut.RoundedReal cf, HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf), 
     Neg cf, Show var, Show cf) =>
    ArithInOut.RoundedSubtr (IntPolyWithConsistentCoeffs var cf) 


instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedMultiplyEffort (IntPolyWithConsistentCoeffs var cf) 
    where
    type ArithInOut.MultEffortIndicator (IntPolyWithConsistentCoeffs var cf) = 
        (ArithInOut.RoundedRealEffortIndicator cf) 
    multDefaultEffort (IntPolyWithConsistentCoeffs (IntPoly cfg _)) = 
        (ArithInOut.roundedRealDefaultEffort sample_cf) 
        where
        sample_cf = (ipolycfg_sample_cf cfg)
    
instance
    (ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf, 
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf),
     Ord var, Show var, Show cf) =>
    ArithInOut.RoundedMultiply (IntPolyWithConsistentCoeffs var cf) 
    where
    multInEff eff p1 p2 = 
        error $ "IntPolyWithConsistentCoeffs does not support inner rounded multiplication"
    multOutEff eff (IntPolyWithConsistentCoeffs p1) (IntPolyWithConsistentCoeffs p2) =
        IntPolyWithConsistentCoeffs $ 
            multThinPolysOut
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
        
instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedPowerToNonnegIntEffort (IntPolyWithConsistentCoeffs var cf)
    where
    type (ArithInOut.PowerToNonnegIntEffortIndicator (IntPolyWithConsistentCoeffs var cf)) =
         ArithInOut.PowerToNonnegIntEffortIndicatorFromMult (IntPolyWithConsistentCoeffs var cf)
    powerToNonnegIntDefaultEffort = ArithInOut.powerToNonnegIntDefaultEffortFromMult

instance
    (ArithInOut.RoundedReal cf, HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf),
     Show var, Show cf, Ord var) =>
    ArithInOut.RoundedPowerToNonnegInt (IntPolyWithConsistentCoeffs var cf) 
    where
    powerToNonnegIntInEff eff p n =
        error $ "IntPolyWithConsistentCoeffs does not support inner rounded integer power"
    powerToNonnegIntOutEff eff (IntPolyWithConsistentCoeffs (IntPoly cfg terms)) n =
        IntPolyWithConsistentCoeffs $ 
            IntPoly cfg $ 
                powThinTermsOut
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
        
        
instance
    (ArithInOut.RoundedReal cf) 
    => 
    ArithInOut.RoundedRingEffort (IntPolyWithConsistentCoeffs var cf)
    where
    type ArithInOut.RingOpsEffortIndicator (IntPolyWithConsistentCoeffs var cf) =
        (ArithInOut.RoundedRealEffortIndicator cf)
    ringOpsDefaultEffort (IntPolyWithConsistentCoeffs (IntPoly cfg _)) = 
        ArithInOut.roundedRealDefaultEffort $ ipolycfg_sample_cf cfg
    ringEffortAdd (IntPolyWithConsistentCoeffs (IntPoly cfg _)) eff = eff  
    ringEffortMult (IntPolyWithConsistentCoeffs (IntPoly cfg _)) eff = eff
    ringEffortPow (IntPolyWithConsistentCoeffs (IntPoly cfg _)) eff = eff  

instance 
    (ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedRing (IntPolyWithConsistentCoeffs var cf)

instance
    (ArithInOut.RoundedMixedAddEffort cf other) => 
    ArithInOut.RoundedMixedAddEffort (IntPolyWithConsistentCoeffs var cf) other 
    where
    type ArithInOut.MixedAddEffortIndicator (IntPolyWithConsistentCoeffs var cf) other = 
        ArithInOut.MixedAddEffortIndicator (IntPoly var cf) other  
    mixedAddDefaultEffort (IntPolyWithConsistentCoeffs p) c =
        ArithInOut.mixedAddDefaultEffort p c

instance
    (ArithInOut.RoundedMixedAdd cf other,  Ord var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedAdd (IntPolyWithConsistentCoeffs var cf) other 
    where
    mixedAddInEff _ _ _ = 
        error $ "IntPolyWithConsistentCoeffs does not support inner rounded addition"
    mixedAddOutEff eff (IntPolyWithConsistentCoeffs p) a =
        IntPolyWithConsistentCoeffs $
            ArithInOut.mixedAddOutEff eff p a 

-- TODO: mixed multiplication and division
-- use scaleTermsOut 
        
--instance
--    (ArithInOut.RoundedMixedDivideEffort cf other,  Ord var, ArithInOut.RoundedReal cf) => 
--    ArithInOut.RoundedMixedDivideEffort (IntPoly var cf) other 
--    where
--    type ArithInOut.MixedDivEffortIndicator (IntPoly var cf) other = 
--        ArithInOut.MixedDivEffortIndicator cf other 
--    mixedDivDefaultEffort (IntPoly cfg _) c = 
--        ArithInOut.mixedDivDefaultEffort (ipolycfg_sample_cf cfg) c
--    
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
--    

instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency cf, 
     Arbitrary cf, 
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedField cf cf,
     Show var,
     Show cf,
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf))
    =>
    NumOrd.RoundedLatticeEffort (IntPolyWithConsistentCoeffs var cf)
    where
    type NumOrd.MinmaxEffortIndicator (IntPolyWithConsistentCoeffs var cf) =
        (MinmaxEffortIndicatorFromRingOps (IntPolyWithConsistentCoeffs var cf) (IntPolyWithConsistentCoeffs var cf),
         Int1To10, -- ^ (degree of Bernstein approximations) - 1   (the degree must be > 1)
         RefOrd.GetEndpointsEffortIndicator (IntPoly var cf))
    minmaxDefaultEffort pw@(IntPolyWithConsistentCoeffs p) =
        (defaultMinmaxEffortIndicatorFromRingOps pw pw,
         Int1To10 3, -- degree 4
         RefOrd.getEndpointsDefaultEffort p)

minmaxUpDnDefaultEffortIntPolyWithBezierDegree degree f =
    (defaultMinmaxEffortIndicatorFromRingOps f f,
     Int1To10 (degree - 1),
     RefOrd.getEndpointsDefaultEffort f)

instance
    (Ord var,
     GeneratableVariables var,
     HasAntiConsistency cf, 
     Arbitrary cf, 
     ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedField cf cf,
     Show var,
     Show cf,
     Show (Imprecision cf),
     NumOrd.PartialComparison (Imprecision cf))
    =>
    NumOrd.RoundedLattice (IntPoly var cf)
    where
    maxUpEff (effMinmax,Int1To10 degreeMinusOne,effGetE) a b =
--        unsafePrint 
--            ( "IntPoly maxUpEff:"
--                ++ "\n a = " ++ showPoly show show a
--                ++ "\n b = " ++ showPoly show show b
--                ++ "\n a `maxUp` b = " ++ showPoly show show result 
--            ) $
        result
        where
        result = maxUpEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aR bR
        (_aL,aR) = RefOrd.getEndpointsOutEff effGetE a
        (_bL,bR) = RefOrd.getEndpointsOutEff effGetE b
    maxDnEff (effMinmax,Int1To10 degreeMinusOne,effGetE) a b =
        result
        where
        result = maxDnEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aL bL
        (aL,_aR) = RefOrd.getEndpointsOutEff effGetE a
        (bL,_bR) = RefOrd.getEndpointsOutEff effGetE b
    minUpEff (effMinmax,Int1To10 degreeMinusOne,effGetE) a b = 
        result
        where
        result = minUpEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aR bR
        (_aL,aR) = RefOrd.getEndpointsOutEff effGetE a
        (_bL,bR) = RefOrd.getEndpointsOutEff effGetE b
    minDnEff (effMinmax,Int1To10 degreeMinusOne,effGetE) a b = 
        result
        where
        result = minDnEffFromRingOps a getX effMinmax (getDegree degreeMinusOne a) aL bL
        (aL,_aR) = RefOrd.getEndpointsOutEff effGetE a
        (bL,_bR) = RefOrd.getEndpointsOutEff effGetE b

getX sizeLimits@(IntPolyCfg vars _ _ sample md ms) =
    newProjection cfg undefined var
    where
    _ = [sizeLimits, cfg] -- , getSizeLimits sampleT]
    var = head vars
    cfg =
        IntPolyCfg [var] [unit] [zero sample] sample md ms
    unit =
        RefOrd.fromEndpointsOutWithDefaultEffort (zero sample, one sample)
    
getDegree degreeMinusOne (IntPoly cfg _) =
    max 2 $ min (degreeMinusOne + 1) maxDeg
    where
    maxDeg = ipolycfg_maxdeg cfg        