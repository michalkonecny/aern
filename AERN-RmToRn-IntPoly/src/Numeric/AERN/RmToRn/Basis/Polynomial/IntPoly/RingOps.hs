{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Measures
import Numeric.AERN.RealArithmetic.Auxiliary

import qualified Numeric.AERN.NumericOrder as NumOrd
import qualified Numeric.AERN.RefinementOrder as RefOrd
import Numeric.AERN.RefinementOrder.OpsImplicitEffort

import Numeric.AERN.Misc.Debug

import qualified Data.IntMap as IntMap

instance
    (ArithInOut.RoundedAddEffort cf) => 
    ArithInOut.RoundedAddEffort (IntPoly var cf) 
    where
    type ArithInOut.AddEffortIndicator (IntPoly var cf) = ArithInOut.AddEffortIndicator cf 
    addDefaultEffort (IntPoly cfg _) = ArithInOut.addDefaultEffort (ipolycfg_sample_cf cfg)
    
instance
    (ArithInOut.RoundedAdd cf, Show var, Show cf) =>
    ArithInOut.RoundedAdd (IntPoly var cf) 
    where
    addInEff eff p1 p2 = error "inner rounded operations not available for IntPoly"
    addOutEff eff p1 p2 = addPolys eff p1 p2

instance
    (ArithInOut.RoundedAdd cf, Neg cf, Show var, Show cf) =>
    ArithInOut.RoundedSubtr (IntPoly var cf) 
    
addPolys :: 
    (ArithInOut.RoundedAdd cf, Show var, Show cf) =>
    ArithInOut.AddEffortIndicator cf -> 
    IntPoly var cf -> IntPoly var cf -> IntPoly var cf
addPolys eff (IntPoly cfg1 poly1) (IntPoly cfg2 poly2)
    =
    let ?addInOutEffort = eff in
    (IntPoly cfg1 $ addTerms poly1 poly2)

--    where
addTerms poly1@(IntPolyC val1) poly2@(IntPolyC val2) = IntPolyC $ val1 <+> val2 
addTerms poly1@(IntPolyV xName1 polys1) poly2@(IntPolyV xName2 polys2)
    = IntPolyV xName2 $ IntMap.unionWith addTerms polys1 polys2 
addTerms p1 p2 =
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
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf),
     Show var, Show cf) =>
    ArithInOut.RoundedMultiply (IntPoly var cf) 
    where
    multInEff eff p1 p2 = error "inner rounded operations not available for IntPoly"
    multOutEff eff p1 p2 = multPolys eff p1 p2
    
multPolys ::
    (ArithInOut.RoundedReal cf, 
     NumOrd.PartialComparison (Imprecision cf), Show (Imprecision cf),
     Show var, Show cf) =>
    (ArithInOut.RoundedRealEffortIndicator cf) -> 
    IntPoly var cf -> IntPoly var cf -> IntPoly var cf
multPolys eff (IntPoly cfg1 poly1) (IntPoly cfg2 poly2)
    =
    let ?addInOutEffort = effAdd in
    let ?multInOutEffort = effMult in
    IntPoly cfg1 $ 
        reduceTermsCount eff cfg1 $ 
            reduceTermsDegree eff cfg1 $ 
                termsNormalise cfg1 $ multTerms poly1 poly2
    where
    effMult = ArithInOut.fldEffortMult sample $ ArithInOut.rrEffortField sample eff
    effPwr = ArithInOut.fldEffortPow sample $ ArithInOut.rrEffortField sample eff
    effAdd = ArithInOut.fldEffortAdd sample $ ArithInOut.rrEffortField sample eff
    sample = ipolycfg_sample_cf cfg1
    

multTerms poly1@(IntPolyC val1) poly2@(IntPolyC val2) = IntPolyC $ val1 <*> val2 
multTerms poly1@(IntPolyV xName1 polys1) poly2@(IntPolyV xName2 polys2)
    =
    IntPolyV xName2 multSubPolys
    where
    multSubPolys 
        = IntMap.fromListWith addTerms $
            [(n1 + n2, multTerms p1 p2) | 
                (n1, p1) <- IntMap.toAscList polys1, 
                (n2, p2) <- IntMap.toAscList polys2 ] 
        
powTerms sample vars = powerFromMult (mkConstTerms (one sample) vars) multTerms

        
instance
    (ArithInOut.RoundedMixedAddEffort cf other) => 
    ArithInOut.RoundedMixedAddEffort (IntPoly var cf) other 
    where
    type ArithInOut.MixedAddEffortIndicator (IntPoly var cf) other = ArithInOut.MixedAddEffortIndicator cf other  
    mixedAddDefaultEffort (IntPoly cfg _) c = 
        ArithInOut.mixedAddDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedAdd cf other,  Ord var, ArithInOut.RoundedReal cf) =>
    ArithInOut.RoundedMixedAdd (IntPoly var cf) other 
    where
    mixedAddInEff eff p1 a = error "inner rounded operations not available for IntPoly"
    mixedAddOutEff eff p1 a = addPolyConst eff p1 a

addPolyConst :: 
    (ArithInOut.RoundedMixedAdd cf other, Ord var, ArithInOut.RoundedReal cf) =>
    ArithInOut.MixedAddEffortIndicator cf other -> 
    IntPoly var cf -> other -> IntPoly var cf
addPolyConst eff (IntPoly cfg poly) const =
    let ?mixedAddInOutEffort = eff in
    IntPoly cfg $ addTermsConst cfg poly const


addTermsConst _ (IntPolyC val) const =
    IntPolyC $ val <+>| const
addTermsConst cfg (IntPolyV x polys) const =
    IntPolyV x $ IntMap.insert 0 newConstPoly polys
    where
    oldConstPoly =
        case IntMap.lookup 0 polys of
            Nothing -> intpoly_terms $ newConstFn cfgR undefined $ zero sampleCf
            Just p -> p
    newConstPoly = addTermsConst cfgR oldConstPoly const
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
    mixedMultOutEff eff p1 a = scalePoly eff p1 a

scalePoly ::
    (ArithInOut.RoundedMixedMultiply cf other) =>
    ArithInOut.MixedMultEffortIndicator cf other -> 
    IntPoly var cf -> other -> IntPoly var cf
scalePoly eff (IntPoly cfg poly) c = 
    let ?mixedMultInOutEffort = eff in
    IntPoly cfg $ scaleTerms c poly 

scaleTerms c (IntPolyC val) =
    IntPolyC $ val <*>| c
scaleTerms c (IntPolyV x polys) = 
    IntPolyV x $ IntMap.map (scaleTerms c) polys

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
    
    
-- a quick and dirty sine implementation; this should be made generic
-- and move to AERN-Real

sinePoly ::
    (ArithInOut.RoundedReal cf,
     NumOrd.PartialComparison (Imprecision cf),  Show (Imprecision cf),
     Show var, Ord var, Show cf) =>
    (ArithInOut.RoundedRealEffortIndicator cf) -> 
    Int {-^ how many terms of the Taylor expansion to consider -} -> 
    IntPoly var cf -> 
    IntPoly var cf
sinePoly eff n x@(IntPoly cfg _) =
--    unsafePrintReturn
--    (
--        "sinePoly:"
--        ++ "\n x = " ++ show x
--        ++ "\n n = " ++ show n
--        ++ "\n result = "
--    ) $
    let (<*>) = multPolys eff in 
    x <*> (aux unitIntPoly (2*n+2)) -- x * (1 - x^2/(2*3)(1 - x^2/(4*5)(1 - ...)))
    where
    unitIntPoly = 
        let (</\>) = RefOrd.meetOutEff effJoin in 
        newConstFn cfg undefined $ (neg o) </\> o
    o = one sampleCf
    aux acc 0 = acc
    aux acc n =
--        unsafePrint
--        (
--            "  sinePoly aux:"
--            ++ "\n    acc = " ++ (show $ checkPoly acc)
--            ++ "\n    n = " ++ show n
--            ++ "\n    squareX = " ++ (show $ checkPoly squareX)
--            ++ "\n    squareXAcc = " ++ (show $ checkPoly squareXAcc)
--            ++ "\n    squareXDivNNPlusOneAcc = " ++ (show $ checkPoly squareXDivNNPlusOneAcc)
--            ++ "\n    negSquareXDivNNPlusOneAcc = " ++ (show $ checkPoly negSquareXDivNNPlusOneAcc)
--            ++ "\n    newAcc = " ++ (show $ checkPoly newAcc)
--        )$
        aux newAcc (n-2)
        where
        newAcc = addPolyConst effAddInt negSquareXDivNNPlusOneAcc (1::Int)
        negSquareXDivNNPlusOneAcc = negPoly squareXDivNNPlusOneAcc
        squareXDivNNPlusOneAcc = divPolyByOther effDivInt squareXAcc (n*(n+1))
        squareXAcc = 
            let (<*>) = multPolys eff in squareX <*> acc 
    squareX =
        let (<*>) = multPolys eff in x <*> x 
    effJoin = ArithInOut.rrEffortJoinMeet sampleCf eff
    effAddInt = ArithInOut.mxfldEffortAdd sampleCf (1::Int) $ ArithInOut.rrEffortIntMixedField sampleCf eff
    effDivInt = ArithInOut.mxfldEffortDiv sampleCf (1::Int) $ ArithInOut.rrEffortIntMixedField sampleCf eff
    sampleCf = ipolycfg_sample_cf cfg
    
