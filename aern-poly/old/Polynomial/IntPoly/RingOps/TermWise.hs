{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps.TermWise
    Description :  term-wise outwards-rounded ring operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Term-wise outwards-rounded ring operations.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps.TermWise
--    (
--    )
where


import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.RingOps.Addition
    
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

import Numeric.AERN.Basics.Consistency

import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap
    
multThinPolysOut ::
    (Show var, Show cf,
     ArithInOut.RoundedReal cf, HasAntiConsistency cf,
     NumOrd.PartialComparison imprecision) 
    =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    (cf -> imprecision) -> 
    IntPoly var cf -> IntPoly var cf -> IntPoly var cf
multThinPolysOut (+) (*) (^) getImpr (IntPoly cfg terms1) (IntPoly _ terms2)
    =
    IntPoly cfg $ 
        reduceTermsCount (+) (*) (^) getImpr varDoms maxSize $ 
            reduceTermsDegree (+) (*) (^) varDoms maxDeg $ 
                termsNormalise $ multThinTermsOut (+) (*) terms1 terms2
    where
    maxSize = ipolycfg_maxsize cfg
    maxDeg = ipolycfg_maxdeg cfg
    varDoms = ipolycfg_domsLZ cfg
    
multThinTermsOut ::
    (Show var, Show cf) 
    =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    IntPolyTerms var cf -> IntPolyTerms var cf -> IntPolyTerms var cf
multThinTermsOut (+) (*) terms1@(IntPolyC val1) terms2@(IntPolyC val2) 
    = 
    IntPolyC $ val1 * val2 
multThinTermsOut (+) (*) terms1@(IntPolyV xName1 powers1) terms2@(IntPolyV xName2 powers2)
    =
    IntPolyV xName2 multSubPolys
    where
    multSubPolys 
        = IntMap.fromListWith (addTerms (+)) $
            [(n1 Prelude.+ n2, multThinTermsOut (+) (*) t1 t2) | 
                (n1, t1) <- IntMap.toAscList powers1,
                (n2, t2) <- IntMap.toAscList powers2 ] 
multThinTermsOut (+) (*) terms1 terms2
    =
    error $ "AERN internal error: multTerms: incompatible operands: "
        ++ "\n terms1 = " ++ show terms1
        ++ "\n terms2 = " ++ show terms2
        
powThinTermsOut ::
    (Show var, Show cf,
     ArithInOut.RoundedReal cf, HasAntiConsistency cf,
     NumOrd.PartialComparison imprecision) 
    =>
    (cf -> cf -> cf) -> 
    (cf -> cf -> cf) -> 
    (cf -> Int -> cf) -> 
    (cf -> imprecision) ->
    cf ->
    (IntPolyCfg var cf) ->
    IntPolyTerms var cf -> Int -> IntPolyTerms var cf
powThinTermsOut (+) (*) (^) getImpr sample cfg = 
    powerFromMult 
        (mkConstTerms (one sample) vars) 
        multTermsReduce
        where
        vars = ipolycfg_vars cfg
        multTermsReduce t1 t2 =
            reduceTermsCount (+) (*) (^) getImpr varDoms maxSize $ 
                reduceTermsDegree (+) (*) (^) varDoms maxDeg $ 
                    termsNormalise $ multThinTermsOut (+) (*) t1 t2
        maxSize = ipolycfg_maxsize cfg
        maxDeg = ipolycfg_maxdeg cfg
        varDoms = ipolycfg_domsLZ cfg

scalePolyByThinOut ::
    (cf -> other -> cf) ->
    other ->
    (IntPoly var cf) -> (IntPoly var cf)
scalePolyByThinOut (*|) c (IntPoly cfg terms) =
    IntPoly cfg  $ scaleTermsByThinOut (*|) c terms
        
scaleTermsByThinOut ::
    (cf -> other -> cf) ->
    other ->
    (IntPolyTerms var cf) -> (IntPolyTerms var cf)
scaleTermsByThinOut (*|) c (IntPolyC val) =
    IntPolyC $ val *| c
scaleTermsByThinOut (*|) c (IntPolyV x polys) = 
    IntPolyV x $ IntMap.map (scaleTermsByThinOut (*|) c) polys

