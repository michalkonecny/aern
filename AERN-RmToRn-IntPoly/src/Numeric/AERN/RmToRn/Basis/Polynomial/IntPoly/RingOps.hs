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
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Reduce

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

import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap

instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedAddEffort (IntPoly var cf) 
    where
    type ArithInOut.AddEffortIndicator (IntPoly var cf) = 
        ArithInOut.RoundedRealEffortIndicator cf 
    addDefaultEffort (IntPoly cfg _) = 
        ArithInOut.roundedRealDefaultEffort (ipolycfg_sample_cf cfg)
    
instance
    (ArithInOut.RoundedReal cf,
     NumOrd.PartialComparison (Imprecision cf), 
     Show var, Show cf) 
    =>
    ArithInOut.RoundedAdd (IntPoly var cf) 
    where
    addInEff eff (IntPoly cfg terms1) (IntPoly _ terms2) =
        IntPoly cfg $ 
            reduceTermsCount (+) (*) (^) (imprecisionOfEff effImpr) cfg $ 
                addTerms (+) terms1 terms2
        where
        (+) = ArithInOut.addInEff effAdd
        (*) = ArithInOut.multInEff effMult
        (^) = ArithInOut.powerToNonnegIntInEff effPwr
        effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
        effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
        effImpr = ArithInOut.rrEffortImprecision sample eff
        sample = ipolycfg_sample_cf cfg
    addOutEff eff (IntPoly cfg terms1) (IntPoly _ terms2) =
        IntPoly cfg $ 
            reduceTermsCount (+) (*) (^) (imprecisionOfEff effImpr) cfg $ 
                addTerms (+) terms1 terms2
        where
        (+) = ArithInOut.addOutEff effAdd
        (*) = ArithInOut.multOutEff effMult
        (^) = ArithInOut.powerToNonnegIntOutEff effPwr
        effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
        effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
        effImpr = ArithInOut.rrEffortImprecision sample eff
        sample = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedReal cf, 
     NumOrd.PartialComparison (Imprecision cf), 
     Neg cf, Show var, Show cf) =>
    ArithInOut.RoundedSubtr (IntPoly var cf) 
    
addTerms :: 
    (Show var, Show cf) =>
    (cf -> cf -> cf) ->
    IntPolyTerms var cf -> IntPolyTerms var cf -> IntPolyTerms var cf
addTerms (+) poly1@(IntPolyC val1) poly2@(IntPolyC val2) = IntPolyC $ val1 + val2 
addTerms (+) poly1@(IntPolyV xName1 polys1) poly2@(IntPolyV xName2 polys2)
    = IntPolyV xName2 $ IntMap.unionWith (addTerms (+)) polys1 polys2 
addTerms (+) p1 p2 =
    error $ "addPolys: cannot add p1=" ++ show p1 ++ " and p2=" ++ show p2

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
     RefOrd.IntervalLike cf, 
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf),
     Ord var, Show var, Show cf) =>
    ArithInOut.RoundedMultiply (IntPoly var cf) 
    where
    multInEff eff p1 p2 = 
        multPolys 
            (ArithInOut.addInEff effAdd) 
            (ArithInOut.multInEff effMult) 
            (ArithInOut.powerToNonnegIntInEff effPwr) 
            (imprecisionOfEff effImpr) 
            p1 p2
        where
        effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
        effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
        effImpr = ArithInOut.rrEffortImprecision sample eff
        sample = getSampleDomValue p1
    multOutEff eff p1 p2 = 
        multPolys 
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
        
    
multPolys ::
    (Show var, Show cf,
     ArithInOut.RoundedReal cf,
     NumOrd.PartialComparison imprecision) =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    (cf -> imprecision) -> 
    IntPoly var cf -> IntPoly var cf -> IntPoly var cf
multPolys (+) (*) (^) getImpr (IntPoly cfg1 poly1) (IntPoly cfg2 poly2)
    =
    IntPoly cfg1 $ 
        reduceTermsCount (+) (*) (^) getImpr cfg1 $ 
            reduceTermsDegree (+) (*) (^) cfg1 $ 
                termsNormalise cfg1 $ multTerms (+) (*) poly1 poly2
    
multTerms ::
    (Show var, Show cf) 
    =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    IntPolyTerms var cf -> IntPolyTerms var cf -> IntPolyTerms var cf
multTerms (+) (*) poly1@(IntPolyC val1) poly2@(IntPolyC val2) 
    = 
    IntPolyC $ val1 * val2 
multTerms (+) (*) poly1@(IntPolyV xName1 polys1) poly2@(IntPolyV xName2 polys2)
    =
    IntPolyV xName2 multSubPolys
    where
    multSubPolys 
        = IntMap.fromListWith (addTerms (+)) $
            [(n1 Prelude.+ n2, multTerms (+) (*) p1 p2) | 
                (n1, p1) <- IntMap.toAscList polys1, 
                (n2, p2) <- IntMap.toAscList polys2 ] 
multTerms (+) (*) poly1 poly2 
    =
    error $ "AERN internal error: multTerms: incompatible operands: "
        ++ "\n poly1 = " ++ show poly1
        ++ "\n poly2 = " ++ show poly2
        
instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedPowerToNonnegIntEffort (IntPoly var cf)
    where
    type (ArithInOut.PowerToNonnegIntEffortIndicator (IntPoly var cf)) =
         ArithInOut.PowerToNonnegIntEffortIndicatorFromMult (IntPoly var cf)
    powerToNonnegIntDefaultEffort = ArithInOut.powerToNonnegIntDefaultEffortFromMult

instance
    (ArithInOut.RoundedReal cf, 
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf),
     Show var, Show cf, Ord var) =>
    ArithInOut.RoundedPowerToNonnegInt (IntPoly var cf) 
    where
    powerToNonnegIntInEff eff (IntPoly cfg terms) n =
        IntPoly cfg $ 
            powTerms 
                (ArithInOut.addInEff effAdd) 
                (ArithInOut.multInEff effMult) 
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
    powerToNonnegIntOutEff eff (IntPoly cfg terms) n = 
        IntPoly cfg $ 
            powTerms 
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
     ArithInOut.RoundedReal cf,
     NumOrd.PartialComparison imprecision) =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    (cf -> imprecision) ->
    cf ->
    (IntPolyCfg var cf) ->
    IntPolyTerms var cf -> Int -> IntPolyTerms var cf
powTerms (+) (*) (^) getImpr sample cfg = 
    powerFromMult 
        (mkConstTerms (one sample) vars) 
        multTermsReduce
        where
        vars = ipolycfg_vars cfg
        multTermsReduce t1 t2 =
            reduceTermsCount (+) (*) (^) getImpr cfg $
            reduceTermsDegree (+) (*) (^) cfg $
            multTerms (+) (*) t1 t2
        
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
     RefOrd.IntervalLike cf,
     Show var, Ord var, Show cf,
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf))
    =>
    ArithInOut.RoundedRing (IntPoly var cf)

        
instance
    (ArithInOut.RoundedMixedAddEffort cf other) => 
    ArithInOut.RoundedMixedAddEffort (IntPoly var cf) other 
    where
    type ArithInOut.MixedAddEffortIndicator (IntPoly var cf) other = ArithInOut.MixedAddEffortIndicator cf other  
    mixedAddDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedAddDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedAdd cf other,  Ord var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedAdd (IntPoly var cf) other 
    where
    mixedAddInEff eff (IntPoly cfg terms) a = 
        IntPoly cfg $ addTermsConst (+|) cfg terms a
        where
        (+|) = ArithInOut.mixedAddInEff eff
    mixedAddOutEff eff (IntPoly cfg terms) a = 
        IntPoly cfg $ addTermsConst (+|) cfg terms a
        where
        (+|) = ArithInOut.mixedAddOutEff eff

addTermsConst (+|) _ (IntPolyC val) const =
    IntPolyC $ val +| const
addTermsConst (+|) cfg (IntPolyV x polys) const =
    IntPolyV x $ IntMap.insert 0 newConstPoly polys
    where
    oldConstPoly =
        case IntMap.lookup 0 polys of
            Nothing -> intpoly_terms $ newConstFn cfgR undefined $ zero sampleCf
            Just p -> p
    newConstPoly = addTermsConst (+|) cfgR oldConstPoly const
    cfgR = cfgRemVar cfg
    sampleCf = ipolycfg_sample_cf cfg
         
instance
    (ArithInOut.RoundedMixedMultiplyEffort cf other) => 
    ArithInOut.RoundedMixedMultiplyEffort (IntPoly var cf) other 
    where
    type ArithInOut.MixedMultEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedMultEffortIndicator cf other 
    mixedMultDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedMultDefaultEffort (ipolycfg_sample_cf cfg) c
    
instance
    (ArithInOut.RoundedMixedMultiply cf other) =>
    ArithInOut.RoundedMixedMultiply (IntPoly var cf) other 
    where
    mixedMultInEff eff p1 a = error "inner rounded operations not available for IntPoly"
    mixedMultOutEff eff (IntPoly cfg terms) a = 
        IntPoly cfg $ scaleTerms (*|) a terms
        where
        (*|) = ArithInOut.mixedMultOutEff eff
        

scaleTerms (*|) c (IntPolyC val) =
    IntPolyC $ val *| c
scaleTerms (*|) c (IntPolyV x polys) = 
    IntPolyV x $ IntMap.map (scaleTerms (*|) c) polys

instance
    (Neg cf) => Neg (IntPoly var cf)
    where
    neg  = negPoly

negPoly ::
    (Neg cf) =>
    IntPoly var cf -> IntPoly var cf
negPoly (IntPoly cfg poly) = 
    IntPoly cfg $ negTerms poly 

negTerms (IntPolyC val) =
    IntPolyC $ neg val
negTerms (IntPolyV x polys) = 
    IntPolyV x $ IntMap.map negTerms polys

instance
    (ArithInOut.RoundedMixedDivideEffort cf other,  Ord var, ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedMixedDivideEffort (IntPoly var cf) other 
    where
    type ArithInOut.MixedDivEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedDivEffortIndicator cf other 
    mixedDivDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedDivDefaultEffort (ipolycfg_sample_cf cfg) c
    
instance
    (ArithInOut.RoundedMixedDivide cf other,  Ord var, ArithInOut.RoundedReal cf) =>
    ArithInOut.RoundedMixedDivide (IntPoly var cf) other 
    where
    mixedDivInEff eff p1 a = error "inner rounded operations not available for IntPoly"
    mixedDivOutEff eff p1 a = divPolyByOther eff p1 a

divPolyByOther ::
    (ArithInOut.RoundedMixedDivide cf other,  Ord var, ArithInOut.RoundedReal cf) =>
    ArithInOut.MixedDivEffortIndicator cf other -> 
    IntPoly var cf -> other -> IntPoly var cf
divPolyByOther eff (IntPoly cfg poly) c = 
    let ?mixedDivInOutEffort = eff in
    IntPoly cfg $ dP poly 
    where
    dP (IntPolyC val) =
        IntPolyC $ val </>| c
    dP (IntPolyV x polys) = 
        IntPolyV x $ IntMap.map dP polys

instance
    (ArithInOut.RoundedReal cf, ArithInOut.RoundedMixedRingEffort cf other, Ord var) 
    => 
    ArithInOut.RoundedMixedRingEffort (IntPoly var cf) other
    where
    type ArithInOut.MixedRingOpsEffortIndicator (IntPoly var cf) other =
        (ArithInOut.RoundedRealEffortIndicator cf,
         ArithInOut.MixedRingOpsEffortIndicator cf other)
    mixedRingOpsDefaultEffort (IntPoly cfg _) sampleOther = 
        (ArithInOut.roundedRealDefaultEffort sampleCf,
         ArithInOut.mixedRingOpsDefaultEffort sampleCf sampleOther)
         where
         sampleCf = ipolycfg_sample_cf cfg
    mxringEffortAdd (IntPoly cfg _) sampleOther (effRR, effMF) =  
        ArithInOut.mxringEffortAdd sampleCf sampleOther effMF
        where
        sampleCf = ipolycfg_sample_cf cfg 
    mxringEffortMult (IntPoly cfg _) sampleOther (effRR, effMF) =  
        ArithInOut.mxringEffortMult sampleCf sampleOther effMF
        where
        sampleCf = ipolycfg_sample_cf cfg 

instance
    (ArithInOut.RoundedReal cf, ArithInOut.RoundedMixedFieldEffort cf other, Ord var) 
    => 
    ArithInOut.RoundedMixedFieldEffort (IntPoly var cf) other
    where
    type ArithInOut.MixedFieldOpsEffortIndicator (IntPoly var cf) other =
        (ArithInOut.RoundedRealEffortIndicator cf,
         ArithInOut.MixedFieldOpsEffortIndicator cf other)
    mixedFieldOpsDefaultEffort (IntPoly cfg _) sampleOther = 
        (ArithInOut.roundedRealDefaultEffort sampleCf,
         ArithInOut.mixedFieldOpsDefaultEffort sampleCf sampleOther)
         where
         sampleCf = ipolycfg_sample_cf cfg
    mxfldEffortAdd (IntPoly cfg _) sampleOther (effRR, effMF) =  
        ArithInOut.mxfldEffortAdd sampleCf sampleOther effMF
        where
        sampleCf = ipolycfg_sample_cf cfg 
    mxfldEffortMult (IntPoly cfg _) sampleOther (effRR, effMF) =  
        ArithInOut.mxfldEffortMult sampleCf sampleOther effMF
        where
        sampleCf = ipolycfg_sample_cf cfg 
    mxfldEffortDiv (IntPoly cfg _) sampleOther (effRR, effMF) =  
        ArithInOut.mxfldEffortDiv sampleCf sampleOther effMF
        where
        sampleCf = ipolycfg_sample_cf cfg 

instance 
    (Ord var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedRing cf other)
    =>
    ArithInOut.RoundedMixedRing (IntPoly var cf) other

instance 
    (Ord var,
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf,
     ArithInOut.RoundedMixedField cf other)
    =>
    ArithInOut.RoundedMixedField (IntPoly var cf) other
    
