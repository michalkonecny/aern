{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Addition
    Description :  universal in/out-rounded polynomial addition
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Universal in/out-rounded polynomial addition, subtraction and negation.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Addition
    (
        addTerms,
        negTerms,
        addTermsConst
    )
where
    
import Prelude hiding ((+))
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Config
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Reduction

import Numeric.AERN.RmToRn.New
--import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
    (AddEffortIndicator, MixedAddEffortIndicator)
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

instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedAddEffort (IntPoly var cf) 
    where
    type AddEffortIndicator (IntPoly var cf) = 
        ArithInOut.RoundedRealEffortIndicator cf 
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
    addInEff eff (IntPoly cfg terms1) (IntPoly _ terms2) =
        reducePolyTermCountIn eff $ 
            IntPoly cfg $
                let ?addInOutEffort = effAdd in
                addTerms (<+>) terms1 terms2
        where
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
        sample = ipolycfg_sample_cf cfg
    addOutEff eff (IntPoly cfg terms1) (IntPoly _ terms2) =
        reducePolyTermCountIn eff $ 
            IntPoly cfg $
                let ?addInOutEffort = effAdd in
                addTerms (>+<) terms1 terms2
        where
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
        sample = ipolycfg_sample_cf cfg

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

addTermsConst :: 
    (Ord var, RefOrd.IntervalLike cf, ArithInOut.RoundedReal cf) 
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
        
{--------- cloned instances for IntPolyWithConsistentCoeffs -----------}        
         
instance
    (ArithInOut.RoundedReal cf) => 
    ArithInOut.RoundedAddEffort (IntPolyWithConsistentCoeffs var cf) 
    where
    type AddEffortIndicator (IntPolyWithConsistentCoeffs var cf) = 
        ArithInOut.AddEffortIndicator (IntPoly var cf)
    addDefaultEffort (IntPolyWithConsistentCoeffs poly) = 
        ArithInOut.addDefaultEffort poly 

instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithInOut.RoundedAdd (IntPolyWithConsistentCoeffs var cf) 
    where
    addOutEff eff (IntPolyWithConsistentCoeffs poly1) (IntPolyWithConsistentCoeffs poly2) =
        IntPolyWithConsistentCoeffs $ ArithInOut.addOutEff eff poly1 poly2
    addInEff = error "AERN internal error: inner rounded addition not available for IntPolyWithConsistentCoeffs"

instance
    (ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf,  
     HasAntiConsistency cf,
     Ord var, 
     Show var, Show cf) 
    =>
    ArithInOut.RoundedSubtr (IntPolyWithConsistentCoeffs var cf) 

instance
    (Neg cf) => Neg (IntPolyWithConsistentCoeffs var cf)
    where
    neg (IntPolyWithConsistentCoeffs poly) = IntPolyWithConsistentCoeffs $ neg poly
    
instance
    (ArithInOut.RoundedMixedAddEffort cf other) => 
    ArithInOut.RoundedMixedAddEffort (IntPolyWithConsistentCoeffs var cf) other 
    where
    type MixedAddEffortIndicator (IntPolyWithConsistentCoeffs var cf) other = 
        ArithInOut.MixedAddEffortIndicator (IntPoly var cf) other
    mixedAddDefaultEffort (IntPolyWithConsistentCoeffs poly) c = 
        ArithInOut.mixedAddDefaultEffort poly c
    
instance
    (ArithInOut.RoundedMixedAdd cf other,  Ord var, 
     ArithInOut.RoundedReal cf, RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedAdd (IntPolyWithConsistentCoeffs var cf) other 
    where
    mixedAddOutEff eff (IntPolyWithConsistentCoeffs poly) a = 
        IntPolyWithConsistentCoeffs $  ArithInOut.mixedAddOutEff eff poly a
    mixedAddInEff = 
        error "AERN internal error: inner rounded mixed addition not available for IntPolyWithConsistentCoeffs"
    