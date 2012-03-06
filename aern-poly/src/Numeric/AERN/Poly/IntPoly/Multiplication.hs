{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Multiplication
    Description :  up/dn/out-rounded polynomial multiplication
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Up/dn/out-rounded polynomial multiplication and scaling.
-}

module Numeric.AERN.Poly.IntPoly.Multiplication
    (
        multTerms,
        scaleTerms
    )
where
    
import Prelude hiding ((+),(*))
import qualified Prelude
    
import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.Reduction
import Numeric.AERN.Poly.IntPoly.Addition

--import Numeric.AERN.RmToRn.New
import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
--import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

--import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Measures
--import Numeric.AERN.RealArithmetic.Auxiliary

--import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsImplicitEffort
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

--import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap

{----- multiplication up/dn via out -----}    

instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMultiplyEffort (IntPoly var cf) 
    where
#if (__GLASGOW_HASKELL__ >= 704)
    type MultEffortIndicator (IntPoly var cf) = 
        (ArithInOut.MultEffortIndicator (IntPoly var cf), 
         RefOrd.GetEndpointsEffortIndicator cf) 
#else
    type ArithUpDn.MultEffortIndicator (IntPoly var cf) = 
        (ArithInOut.MultEffortIndicator (IntPoly var cf),
         RefOrd.GetEndpointsEffortIndicator cf) 
#endif
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
        snd $ polyGetEndpointsOut effGetE $ ArithInOut.multOutEff effOut p1 p2
    multDnEff (effOut, effGetE) p1 p2 =
        fst $ polyGetEndpointsOut effGetE $ ArithInOut.multOutEff effOut p1 p2

polyGetEndpointsOut ::
    RefOrd.IntervalLike cf 
    =>
    RefOrd.GetEndpointsEffortIndicator cf -> 
    IntPoly var cf -> 
    (IntPoly var cf, IntPoly var cf)
polyGetEndpointsOut effGetE p =
    polySplitWith (RefOrd.getEndpointsOutEff effGetE) p

instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedMultiplyEffort (IntPoly var cf) 
    where
#if (__GLASGOW_HASKELL__ >= 704)
    type MultEffortIndicator (IntPoly var cf) = 
        ArithInOut.RoundedRealEffortIndicator cf 
#else
    type ArithInOut.MultEffortIndicator (IntPoly var cf) = 
        ArithInOut.RoundedRealEffortIndicator cf 
#endif
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
                let ?addInOutEffort = effAdd in
                let ?multInOutEffort = effMult in
                termsNormalise $ multTerms (<+>) (<*>) terms1 terms2
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


    
{----- mixed addition up/dn via out -----}    

{- 
    The generic up/dn rounded mixed addition commented out below
    conflicts with the generic instance of mixed addition 
    e (Interval e) which is used to define the pseudo-mixed addition
    of (Interval e) (Interval e)
    
    We therefore have to make do with a few concrete instances for the common types. 
-}

----instance
----    (ArithInOut.RoundedMixedMultiplyEffort cf other, 
----     RefOrd.IntervalLike cf) 
----    => 
----    ArithUpDn.RoundedMixedMultiplyEffort (IntPoly var cf) other 
----    where
----if (__GLASGOW_HASKELL__ >= 704)
----    type MixedAddEffortIndicator (IntPoly var cf) other = 
----        (ArithInOut.MixedAddEffortIndicator (IntPoly var cf) other, 
----         RefOrd.GetEndpointsEffortIndicator cf) 
----else
----    type ArithUpDn.MixedAddEffortIndicator (IntPoly var cf) other = 
----        (ArithInOut.MixedAddEffortIndicator (IntPoly var cf) other,
----         RefOrd.GetEndpointsEffortIndicator cf) 
----endif
----    mixedAddDefaultEffort p@(IntPoly cfg _) sampleOther = 
----        (ArithInOut.mixedAddDefaultEffort p sampleOther,
----         RefOrd.getEndpointsDefaultEffort sampleCf)
----        where
----        sampleCf = ipolycfg_sample_cf cfg
----
----instance
----    (ArithInOut.RoundedMixedMultiply cf other,
----     ArithInOut.RoundedReal cf,
----     RefOrd.IntervalLike cf,  
----     HasAntiConsistency cf,
----     Ord var, 
----     Show var, Show cf) 
----    =>
----    ArithUpDn.RoundedMixedMultiply (IntPoly var cf) other 
----    where
----    mixedAddUpEff (effOut, effGetE) p1 other =
----        snd $ polyGetEndpointsOut effGetE $ ArithInOut.mixedAddOutEff effOut p1 other
----    mixedAddDnEff (effOut, effGetE) p1 other =
----        fst $ polyGetEndpointsOut effGetE $ ArithInOut.mixedAddOutEff effOut p1 other

instance
    (ArithInOut.RoundedMixedMultiplyEffort cf Int,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedMultiplyEffort (IntPoly var cf) Int 
    where
#if (__GLASGOW_HASKELL__ >= 704)
    type MixedMultEffortIndicator (IntPoly var cf) Int = 
        (ArithInOut.MixedMultEffortIndicator (IntPoly var cf) Int, 
         RefOrd.GetEndpointsEffortIndicator cf) 
#else
    type ArithUpDn.MixedMultEffortIndicator (IntPoly var cf) Int = 
        (ArithInOut.MixedMultEffortIndicator (IntPoly var cf) Int,
         RefOrd.GetEndpointsEffortIndicator cf) 
#endif
    mixedMultDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedMultDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

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
        snd $ polyGetEndpointsOut effGetE $ ArithInOut.mixedMultOutEff effOut p1 other
    mixedMultDnEff (effOut, effGetE) p1 other =
        fst $ polyGetEndpointsOut effGetE $ ArithInOut.mixedMultOutEff effOut p1 other
    
instance
    (ArithInOut.RoundedMixedMultiplyEffort cf other) => 
    ArithInOut.RoundedMixedMultiplyEffort (IntPoly var cf) other 
    where
#if (__GLASGOW_HASKELL__ >= 704)
    type MixedMultEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedMultEffortIndicator cf other  
#else
    type ArithInOut.MixedMultEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedMultEffortIndicator cf other  
#endif
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
        
