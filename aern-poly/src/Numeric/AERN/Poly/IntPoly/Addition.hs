{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Addition
    Description :  up/dn/out-rounded polynomial addition
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Up/dn/out-rounded polynomial addition, subtraction and negation.
-}

module Numeric.AERN.Poly.IntPoly.Addition
    (
        addTerms,
        negTerms,
        addTermsConst
    )
where
    
import Prelude hiding ((+))
    
import Numeric.AERN.Poly.IntPoly.Config
import Numeric.AERN.Poly.IntPoly.IntPoly
import Numeric.AERN.Poly.IntPoly.Reduction

import Numeric.AERN.RmToRn.New
--import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
--import Numeric.AERN.RealArithmetic.NumericOrderRounding.OpsImplicitEffort

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Measures
--import Numeric.AERN.RealArithmetic.Auxiliary

--import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsImplicitEffort
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

--import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap

{----- addition up/dn via out -----}    

instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedAddEffort (IntPoly var cf) 
    where
#if (__GLASGOW_HASKELL__ >= 704)
    type AddEffortIndicator (IntPoly var cf) = 
        (ArithInOut.AddEffortIndicator (IntPoly var cf), 
         RefOrd.GetEndpointsEffortIndicator cf) 
#else
    type ArithUpDn.AddEffortIndicator (IntPoly var cf) = 
        (ArithInOut.AddEffortIndicator (IntPoly var cf),
         RefOrd.GetEndpointsEffortIndicator cf) 
#endif
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
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedAddEffort (IntPoly var cf) 
    where
#if (__GLASGOW_HASKELL__ >= 704)
    type AddEffortIndicator (IntPoly var cf) = 
        ArithInOut.RoundedRealEffortIndicator cf 
#else
    type ArithInOut.AddEffortIndicator (IntPoly var cf) = 
        ArithInOut.RoundedRealEffortIndicator cf 
#endif
    addDefaultEffort (IntPoly cfg _) = 
        ArithInOut.roundedRealDefaultEffort (ipolycfg_sample_cf cfg)
    
instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithInOut.RoundedAdd (IntPoly var cf) 
    where
    addOutEff eff (IntPoly cfg terms1) (IntPoly _ terms2) =
        reducePolyTermCountOut eff $ 
            IntPoly cfg $
                let ?addInOutEffort = effAdd in
                addTerms (<+>) terms1 terms2
        where
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
        sample = ipolycfg_sample_cf cfg
    addInEff =
        error "aern-poly: IntPoly does not support inwards-rounded addition" 
--    addInEff eff (IntPoly cfg terms1) (IntPoly _ terms2) =
--        reducePolyTermCountIn eff $ 
--            IntPoly cfg $
--                let ?addInOutEffort = effAdd in
--                addTerms (<+>) terms1 terms2
--        where
--        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
--        sample = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithInOut.RoundedSubtr (IntPoly var cf) 
    
addTerms :: 
    (Show var, Show cf) =>
    (cf -> cf -> cf) ->
    IntPolyTerms var cf -> IntPolyTerms var cf -> IntPolyTerms var cf
addTerms (+) (IntPolyC val1) (IntPolyC val2) = IntPolyC $ val1 + val2 
addTerms (+) (IntPolyV _xName1 powers1) (IntPolyV xName2 powers2)
    = IntPolyV xName2 $ IntMap.unionWith (addTerms (+)) powers1 powers2 
addTerms _ t1 t2 =
    error $ "addTerms: cannot add t1=" ++ show t1 ++ " and t2=" ++ show t2

{----- negation -----}    


instance
    (Neg cf) => Neg (IntPoly var cf)
    where
    neg (IntPoly cfg terms) = IntPoly cfg $ negTerms terms

negTerms ::
    Neg cf =>
    IntPolyTerms var cf -> IntPolyTerms var cf
negTerms (IntPolyC val) =
    IntPolyC $ neg val
negTerms (IntPolyV x polys) = 
    IntPolyV x $ IntMap.map negTerms polys
    
  
{----- mixed addition up/dn via out -----}    

{- 
    The generic up/dn rounded mixed addition commented out below
    conflicts with the generic instance of mixed addition 
    e (Interval e) which is used to define the pseudo-mixed addition
    of (Interval e) (Interval e)
    
    We therefore have to make do with a few concrete instances for the common types. 
-}

----instance
----    (ArithInOut.RoundedMixedAddEffort cf other, 
----     RefOrd.IntervalLike cf) 
----    => 
----    ArithUpDn.RoundedMixedAddEffort (IntPoly var cf) other 
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
----    (ArithInOut.RoundedMixedAdd cf other,
----     ArithInOut.RoundedReal cf,
----     RefOrd.IntervalLike cf,  
----     HasAntiConsistency cf,
----     Ord var, 
----     Show var, Show cf) 
----    =>
----    ArithUpDn.RoundedMixedAdd (IntPoly var cf) other 
----    where
----    mixedAddUpEff (effOut, effGetE) p1 other =
----        snd $ polyGetEndpointsOut effGetE $ ArithInOut.mixedAddOutEff effOut p1 other
----    mixedAddDnEff (effOut, effGetE) p1 other =
----        fst $ polyGetEndpointsOut effGetE $ ArithInOut.mixedAddOutEff effOut p1 other

instance
    (ArithInOut.RoundedMixedAddEffort cf Int,
     RefOrd.IntervalLike cf) 
    => 
    ArithUpDn.RoundedMixedAddEffort (IntPoly var cf) Int 
    where
#if (__GLASGOW_HASKELL__ >= 704)
    type MixedAddEffortIndicator (IntPoly var cf) Int = 
        (ArithInOut.MixedAddEffortIndicator (IntPoly var cf) Int, 
         RefOrd.GetEndpointsEffortIndicator cf) 
#else
    type ArithUpDn.MixedAddEffortIndicator (IntPoly var cf) Int = 
        (ArithInOut.MixedAddEffortIndicator (IntPoly var cf) Int,
         RefOrd.GetEndpointsEffortIndicator cf) 
#endif
    mixedAddDefaultEffort p@(IntPoly cfg _) sampleOther = 
        (ArithInOut.mixedAddDefaultEffort p sampleOther,
         RefOrd.getEndpointsDefaultEffort sampleCf)
        where
        sampleCf = ipolycfg_sample_cf cfg

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
    (ArithInOut.RoundedMixedAddEffort cf other) => 
    ArithInOut.RoundedMixedAddEffort (IntPoly var cf) other 
    where
#if (__GLASGOW_HASKELL__ >= 704)
    type MixedAddEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedAddEffortIndicator cf other  
#else
    type ArithInOut.MixedAddEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedAddEffortIndicator cf other  
#endif
    mixedAddDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedAddDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedMixedAdd cf other,   
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedAdd (IntPoly var cf) other 
    where
    mixedAddOutEff eff (IntPoly cfg terms) a = 
        IntPoly cfg $ addTermsConst (+|) cfg terms a
        where
        (+|) = ArithInOut.mixedAddOutEff eff
    mixedAddInEff =
        error "aern-poly: IntPoly does not support inwards-rounded mixed addition" 

addTermsConst :: 
    (Ord var, Show var, Show cf,
     RefOrd.IntervalLike cf,
     HasConsistency cf, 
    ArithInOut.RoundedReal cf) 
    =>
    (cf -> t -> cf) -> 
    IntPolyCfg var cf -> 
    IntPolyTerms var cf ->
     t -> 
     IntPolyTerms var cf
addTermsConst (+|) _ (IntPolyC val) constant =
    IntPolyC $ val +| constant
addTermsConst (+|) cfg (IntPolyV x polys) constant =
    IntPolyV x $ IntMap.insert 0 newConstPoly polys
    where
    oldConstPoly =
        case IntMap.lookup 0 polys of
            Nothing -> intpoly_terms $ newConstFn cfgR undefined $ zero sampleCf
            Just p -> p
    newConstPoly = addTermsConst (+|) cfgR oldConstPoly constant
    cfgR = cfgRemVar cfg
    sampleCf = ipolycfg_sample_cf cfg
        
