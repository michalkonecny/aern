{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.ConsistentCoeffs.Multiplication
    Description :  out-rounded consistent coeffs multiplication
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Out-rounded multiplication of polynomials with consistent coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.ConsistentCoeffs.Multiplication
--    (
--    )
where
    
import Prelude hiding ((+),(*))
import qualified Prelude
    
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Config
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Reduction
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Addition

--import Numeric.AERN.RmToRn.New
--import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
    (MultEffortIndicator)
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

--import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
--import Numeric.AERN.RealArithmetic.Auxiliary

--import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsImplicitEffort
import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Basics.Consistency

--import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap

instance
    (ArithInOut.RoundedReal cf) 
    => 
    ArithInOut.RoundedMultiplyEffort (IntPolyWithConsistentCoeffs var cf) 
    where
    type MultEffortIndicator (IntPolyWithConsistentCoeffs var cf) = 
        (ArithInOut.RoundedRealEffortIndicator cf) 
    multDefaultEffort (IntPolyWithConsistentCoeffs (IntPoly cfg _)) = 
        (ArithInOut.roundedRealDefaultEffort sample_cf) 
        where
        sample_cf = ipolycfg_sample_cf cfg

instance
    (ArithInOut.RoundedReal cf,
     HasAntiConsistency cf,
     RefOrd.IntervalLike cf, 
     Show (Imprecision cf),
     Ord var, Show var, Show cf) =>
    ArithInOut.RoundedMultiply (IntPolyWithConsistentCoeffs var cf) 
    where
    multOutEff eff 
            (IntPolyWithConsistentCoeffs (IntPoly cfg terms1)) 
            (IntPolyWithConsistentCoeffs (IntPoly _ terms2)) 
        =
        IntPolyWithConsistentCoeffs $ 
            reducePolyTermCountOut eff $ 
                reducePolyDegreeOut eff $
                    IntPoly cfg $
                        let ?addInOutEffort = effAdd in
                        let ?multInOutEffort = effMult in
                        termsNormalise $ multTerms (<+>) (<*>) terms1 terms2
        where
        effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
        effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
        sample = ipolycfg_sample_cf cfg
    multInEff = 
        error "AERN internal error: inner rounded multiplication not available for IntPolyWithConsistentCoeffs"


multTerms ::
    (Show var, Show cf) 
    =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    IntPolyTerms var cf -> IntPolyTerms var cf -> IntPolyTerms var cf
multTerms _ (*) (IntPolyC val1) (IntPolyC val2) 
    = 
    IntPolyC $ val1 * val2 
multTerms (+) (*) (IntPolyV _xName1 powers1) (IntPolyV xName2 powers2)
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

        
-- TODO: add also scaling by a coefficient
        