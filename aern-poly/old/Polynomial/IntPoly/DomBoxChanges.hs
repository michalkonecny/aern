{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.DomBoxChanges
    Description :  restructuring of the domain box polynomials  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Datatype of polynomials with interval coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.DomBoxChanges
--    (
--    )
where

import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Config
import Numeric.AERN.RmToRn.Basis.Polynomial.IntPoly.Poly
    
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
--import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsDefaultEffort

import qualified Numeric.AERN.RefinementOrder as RefOrd
--import Numeric.AERN.RefinementOrder.OpsDefaultEffort

import qualified Data.IntMap as IntMap

import Data.List (sortBy)
    
polyAddMainVar ::
    (ArithInOut.RoundedReal cf, 
     RefOrd.IntervalLike cf)
    =>
    RefOrd.GetEndpointsEffortIndicator cf -> 
    (ArithInOut.RoundedRealEffortIndicator cf) -> 
    var ->
    cf -> 
    IntPoly var cf -> 
    IntPoly var cf
polyAddMainVar effGetE effCF var dom _p@(IntPoly cfg terms) =
    IntPoly cfgNew termsNew
    where
    termsNew = IntPolyV var $ IntMap.singleton 0 terms
    cfgNew = cfg 
        { 
            ipolycfg_vars = var : ipolycfg_vars cfg, 
            ipolycfg_domsLZ = domLZ : ipolycfg_domsLZ cfg, 
            ipolycfg_domsLE = domLE : ipolycfg_domsLE cfg 
        }
    (domLZ, domLE) = domToDomLZLE effGetE effCF dom
--    sampleCf = ipolycfg_sample_cf cfg
--    _ = [sampleCf, dom]



polyRenameMainVar :: var -> IntPoly var cf -> IntPoly var cf
polyRenameMainVar newVarName _p@(IntPoly cfg (IntPolyV _ coeffs)) =
    IntPoly cfgNew termsNew
    where
    termsNew = IntPolyV newVarName coeffs
    cfgNew = cfg 
        { 
            ipolycfg_vars = newVarName : (tail $ ipolycfg_vars cfg) 
        }
polyRenameMainVar _ _ =
    error "aern-intpoly: DomBoxChanges.polyRenameMainVar called with a zero-variate polynomial"

polySwapFirstTwoVars :: IntPoly var cf -> IntPoly var cf
polySwapFirstTwoVars (IntPoly cfg terms) =
    IntPoly cfgSwapped $ swappedTerms
    where
    swappedTerms = termsSwapFirstTwoVars vars terms
    vars = ipolycfg_vars cfg
    domsLZ = ipolycfg_domsLZ cfg
    domsLE = ipolycfg_domsLE cfg
    cfgSwapped =
        cfg 
            {
                ipolycfg_vars = swapFirstTwo vars, 
                ipolycfg_domsLZ = swapFirstTwo domsLZ, 
                ipolycfg_domsLE = swapFirstTwo domsLE
            }
    swapFirstTwo :: [a] -> [a]
    swapFirstTwo (e1 : e2 : rest) = e2 : e1 : rest
    swapFirstTwo _ = error "aern-intpoly: internal error in DomBoxChanges.polySwapFirstTwoVars"

termsSwapFirstTwoVars :: [var] -> IntPolyTerms var cf -> IntPolyTerms var cf
termsSwapFirstTwoVars (var1 : var2 : _) (IntPolyV _ polys) = 
    IntPolyV var2 $ newPolys
    where
    newPolys =
        IntMap.fromAscList $
            map mkTerms $ groupByFst sortedSwappedPowerAssocs
        where
        mkTerms (n1, assocs) = (n1, IntPolyV var1 $ IntMap.fromAscList assocs) 
    sortedSwappedPowerAssocs = sortBy compareFst swappedPowerAssocs
        where
        compareFst (a,_) (b,_) = compare a b
    swappedPowerAssocs = [(n2, (n1, coeff)) | (n1, list1) <- powerAssocs, (n2, coeff) <- list1]
    powerAssocs = 
        map (\(n1, IntPolyV _ polys2) -> (n1, IntMap.toAscList polys2)) $ 
            IntMap.toAscList polys
termsSwapFirstTwoVars _ _ =
    error "aern-intpoly internal error: DomBoxChanges.termsSwapFirstTwoVars called with less than two variables"

groupByFst :: (Eq a) => [(a,b)] -> [(a,[b])]
groupByFst [] = []
groupByFst ((key, val) : assocs) = aux key [val] assocs
    where
    aux prevKey prevVals [] = [(prevKey, reverse prevVals)]
    aux prevKey prevVals ((key2, val2) : rest)
        | key2 == prevKey = aux prevKey (val2 : prevVals) rest
        | otherwise = (prevKey, reverse prevVals) : (aux key2 [val2] rest)

            