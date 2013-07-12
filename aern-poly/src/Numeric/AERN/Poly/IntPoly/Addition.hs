{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
    Module      :  Numeric.AERN.Poly.IntPoly.Addition
    Description :  out-rounded polynomial addition
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Out-rounded polynomial addition, subtraction and negation.
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
import Numeric.AERN.Poly.IntPoly.New

--import Numeric.AERN.RmToRn.New
--import Numeric.AERN.RmToRn.Domain

--import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
    (AddEffortIndicator, MixedAddEffortIndicator)

import Numeric.AERN.RealArithmetic.ExactOps
--import Numeric.AERN.RealArithmetic.Measures
--import Numeric.AERN.RealArithmetic.Auxiliary

--import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Basics.Interval
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
    addOutEff eff (IntPoly cfg terms1) (IntPoly _ terms2) =
        reducePolyTermCountOut eff $ 
            IntPoly cfg $ addTerms (<+>) terms1 terms2
        where
        (<+>) = ArithInOut.addOutEff effAdd
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
    
  
instance
    (ArithInOut.RoundedMixedAddEffort cf Integer) => 
    ArithInOut.RoundedMixedAddEffort (IntPoly var cf) Integer 
    where
    type MixedAddEffortIndicator (IntPoly var cf) Integer = 
        ArithInOut.MixedAddEffortIndicator cf Integer  
    mixedAddDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedAddDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedMixedAdd cf Integer,   
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedAdd (IntPoly var cf) Integer 
    where
    mixedAddOutEff = mixedAddOutEffGeneric
    mixedAddInEff = mixedAddInEffGeneric
    
instance
    (ArithInOut.RoundedMixedAddEffort cf Int) => 
    ArithInOut.RoundedMixedAddEffort (IntPoly var cf) Int 
    where
    type MixedAddEffortIndicator (IntPoly var cf) Int = 
        ArithInOut.MixedAddEffortIndicator cf Int  
    mixedAddDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedAddDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedMixedAdd cf Int,   
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedAdd (IntPoly var cf) Int 
    where
    mixedAddOutEff = mixedAddOutEffGeneric
    mixedAddInEff = mixedAddInEffGeneric
    
instance
    (ArithInOut.RoundedMixedAddEffort cf Rational) => 
    ArithInOut.RoundedMixedAddEffort (IntPoly var cf) Rational 
    where
    type MixedAddEffortIndicator (IntPoly var cf) Rational = 
        ArithInOut.MixedAddEffortIndicator cf Rational  
    mixedAddDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedAddDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedMixedAdd cf Rational,   
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedAdd (IntPoly var cf) Rational 
    where
    mixedAddOutEff = mixedAddOutEffGeneric
    mixedAddInEff = mixedAddInEffGeneric
    
instance
    (ArithInOut.RoundedMixedAddEffort cf Double) => 
    ArithInOut.RoundedMixedAddEffort (IntPoly var cf) Double 
    where
    type MixedAddEffortIndicator (IntPoly var cf) Double = 
        ArithInOut.MixedAddEffortIndicator cf Double  
    mixedAddDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedAddDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedMixedAdd cf Double,   
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedAdd (IntPoly var cf) Double 
    where
    mixedAddOutEff = mixedAddOutEffGeneric
    mixedAddInEff = mixedAddInEffGeneric
    
instance
    (ArithInOut.RoundedMixedAddEffort cf (Interval e)) => 
    ArithInOut.RoundedMixedAddEffort (IntPoly var cf) (Interval e) 
    where
    type MixedAddEffortIndicator (IntPoly var cf) (Interval e) = 
        ArithInOut.MixedAddEffortIndicator cf (Interval e)  
    mixedAddDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedAddDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (Ord var, Show var, Show cf,
     ArithInOut.RoundedMixedAdd cf (Interval e),   
     ArithInOut.RoundedReal cf, 
     HasConsistency cf, 
     RefOrd.IntervalLike cf) 
    =>
    ArithInOut.RoundedMixedAdd (IntPoly var cf) (Interval e) 
    where
    mixedAddOutEff = mixedAddOutEffGeneric
    mixedAddInEff = mixedAddInEffGeneric
    
mixedAddOutEffGeneric, mixedAddInEffGeneric :: 
    (Ord var, Show cf, Show var, ArithInOut.RoundedReal cf,
     RefOrd.IntervalLike cf, HasConsistency cf,
     ArithInOut.RoundedMixedAdd cf t) 
    =>
    MixedAddEffortIndicator cf t
    -> IntPoly var cf -> t -> IntPoly var cf
mixedAddOutEffGeneric eff (IntPoly cfg terms) a = 
    IntPoly cfg $ addTermsConst (+|) cfg terms a
    where
    (+|) = ArithInOut.mixedAddOutEff eff
mixedAddInEffGeneric =
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
            Nothing -> mkConstTerms (zero sampleCf) varsR
            Just p -> p
    newConstPoly = addTermsConst (+|) cfgR oldConstPoly constant
    varsR = ipolycfg_vars cfg
    cfgR = cfgRemFirstVar cfg
    sampleCf = ipolycfg_sample_cf cfg
        
