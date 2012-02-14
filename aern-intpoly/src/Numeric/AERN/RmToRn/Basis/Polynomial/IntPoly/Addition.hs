{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
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
--    (
--    )
where
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Basics

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
    ArithInOut.RoundedAddEffort (IntPoly var cf) 
    where
    type ArithInOut.AddEffortIndicator (IntPoly var cf) = 
        ArithInOut.RoundedRealEffortIndicator cf 
    addDefaultEffort (IntPoly cfg _) = 
        ArithInOut.roundedRealDefaultEffort (ipolycfg_sample_cf cfg)
    
instance
    (ArithInOut.RoundedReal cf,  HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf), 
     Show var, Show cf) 
    =>
    ArithInOut.RoundedAdd (IntPoly var cf) 
    where
    addInEff eff (IntPoly cfg terms1) (IntPoly _ terms2) =
        IntPoly cfg $ 
            reduceTermsCount (+) (*) (^) (imprecisionOfEff effImpr) varDoms maxSize $ 
                addTerms (+) terms1 terms2
        where
        maxSize = ipolycfg_maxsize cfg
        varDoms = map flipConsistency $ ipolycfg_domsLZ cfg
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
            reduceTermsCount (+) (*) (^) (imprecisionOfEff effImpr) varDoms maxSize $ 
                addTerms (+) terms1 terms2
        where
        maxSize = ipolycfg_maxsize cfg
        varDoms = ipolycfg_domsLZ cfg
        (+) = ArithInOut.addOutEff effAdd
        (*) = ArithInOut.multOutEff effMult
        (^) = ArithInOut.powerToNonnegIntOutEff effPwr
        effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
        effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
        effImpr = ArithInOut.rrEffortImprecision sample eff
        sample = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedReal cf, HasAntiConsistency cf,
     NumOrd.PartialComparison (Imprecision cf), 
     Neg cf, Show var, Show cf) =>
    ArithInOut.RoundedSubtr (IntPoly var cf) 
    
addTerms :: 
    (Show var, Show cf) =>
    (cf -> cf -> cf) ->
    IntPolyTerms var cf -> IntPolyTerms var cf -> IntPolyTerms var cf
addTerms (+) terms1@(IntPolyC val1) terms2@(IntPolyC val2) = IntPolyC $ val1 + val2 
addTerms (+) terms1@(IntPolyV xName1 powers1) terms2@(IntPolyV xName2 powers2)
    = IntPolyV xName2 $ IntMap.unionWith (addTerms (+)) powers1 powers2 
addTerms (+) t1 t2 =
    error $ "addTerms: cannot add t1=" ++ show t1 ++ " and t2=" ++ show t2

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
         
    