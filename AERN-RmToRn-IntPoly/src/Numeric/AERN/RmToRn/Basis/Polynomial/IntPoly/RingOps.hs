{-# LANGUAGE TypeFamilies #-}
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

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps


instance
    (ArithInOut.RoundedAddEffort cf) => 
    ArithInOut.RoundedAddEffort (IntPoly var cf) 
    where
    type ArithInOut.AddEffortIndicator (IntPoly var cf) = ArithInOut.AddEffortIndicator cf 
    addDefaultEffort (IntPolyG cfg _ _) = ArithInOut.addDefaultEffort (ipolycfg_sample_cf cfg)
    addDefaultEffort (IntPolyV cfg _ _) = ArithInOut.addDefaultEffort (ipolycfg_sample_cf cfg)
    
instance
    (ArithInOut.RoundedAdd cf) =>
    ArithInOut.RoundedAdd (IntPoly var cf) 
    where
    addInEff eff p1 p2 = error "inner rounded operations not available for IntPoly"
    addOutEff eff p1 p2 = addPolys eff p1 p2
    
addPolys :: 
    (ArithInOut.RoundedAdd cf) =>
    ArithInOut.AddEffortIndicator cf -> 
    IntPoly var cf -> IntPoly var cf -> IntPoly var cf
addPolys eff poly1@(IntPolyG cfg xName1 coeffs1) poly2@(IntPolyG _ xName2 coeffs2) 
    | lengthPoly1 == lengthPoly2 =
        let ?addInOutEffort = eff in
        IntPolyG cfg xName1 $ zipWith (<+>) coeffs1 coeffs2
    | lengthPoly1 > lengthPoly2 =
        let ?addInOutEffort = eff in
        IntPolyG cfg xName1 $ highCoeffs1 ++ (zipWith (<+>) lowCoeffs1  coeffs2)
    | otherwise =
        let ?addInOutEffort = eff in
        IntPolyG cfg xName1 $ highCoeffs2 ++ (zipWith (<+>) lowCoeffs2  coeffs1)
    where
    (highCoeffs2, lowCoeffs2) = splitAt (lengthPoly2 - lengthPoly1) coeffs2
    (highCoeffs1, lowCoeffs1) = splitAt (lengthPoly1 - lengthPoly2) coeffs1
    lengthPoly1 = length coeffs1
    lengthPoly2 = length coeffs2
addPolys eff poly1@(IntPolyV cfg xName1 polys1) poly2@(IntPolyV _ xName2 polys2) 
    | lengthPoly1 == lengthPoly2 =
        IntPolyV cfg xName1 $ zipWith (addPolys eff) polys1 polys2
    | lengthPoly1 > lengthPoly2 =
        IntPolyV cfg xName1 $ highPolys1 ++ (zipWith (addPolys eff) lowPolys1  polys2)
    | otherwise =
        IntPolyV cfg xName1 $ highPolys2 ++ (zipWith (addPolys eff) lowPolys2  polys1)
    where
    (highPolys2, lowPolys2) = splitAt (lengthPoly2 - lengthPoly1) polys2
    (highPolys1, lowPolys1) = splitAt (lengthPoly1 - lengthPoly2) polys1
    lengthPoly1 = length polys1
    lengthPoly2 = length polys2
    
instance
    (ArithInOut.RoundedMixedAddEffort cf other) => 
    ArithInOut.RoundedMixedAddEffort (IntPoly var cf) other 
    where
    type ArithInOut.MixedAddEffortIndicator (IntPoly var cf) other = ArithInOut.MixedAddEffortIndicator cf other  
    mixedAddDefaultEffort (IntPolyG cfg _ _) c = 
        ArithInOut.mixedAddDefaultEffort (ipolycfg_sample_cf cfg) c
    mixedAddDefaultEffort (IntPolyV cfg _ _) c = 
        ArithInOut.mixedAddDefaultEffort (ipolycfg_sample_cf cfg) c

instance
    (ArithInOut.RoundedMixedAdd cf other, HasZero cf) =>
    ArithInOut.RoundedMixedAdd (IntPoly var cf) other 
    where
    mixedAddInEff eff p1 a = error "inner rounded operations not available for IntPoly"
    mixedAddOutEff eff p1 a = addPolyConst eff p1 a

addPolyConst :: 
    (ArithInOut.RoundedMixedAdd cf other, HasZero cf) =>
    ArithInOut.MixedAddEffortIndicator cf other -> 
    IntPoly var cf -> other -> IntPoly var cf
addPolyConst eff (IntPolyG cfg x []) const = 
    let ?mixedAddInOutEffort = eff in
    IntPolyG cfg x [zero <+>| const]
addPolyConst eff (IntPolyG cfg x coeffs) const =
    let ?mixedAddInOutEffort = eff in
    IntPolyG cfg x $ aux coeffs
    where
    aux [c] = [c <+>| const]
    aux (t : ts) = t : (aux ts)  
addPolyConst eff (IntPolyV cfg x polys) const =
    IntPolyV cfg x $ aux polys
    where
    aux [p] = [addPolyConst eff p const]
    aux (t : ts) = t : (aux ts)  
    
instance
    (ArithInOut.RoundedMixedMultiplyEffort cf other) => 
    ArithInOut.RoundedMixedMultiplyEffort (IntPoly var cf) other 
    where
    type ArithInOut.MixedMultEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedMultEffortIndicator cf other 
    mixedMultDefaultEffort (IntPolyG cfg _ _) c = 
        ArithInOut.mixedMultDefaultEffort (ipolycfg_sample_cf cfg) c
    mixedMultDefaultEffort (IntPolyV cfg _ _) c = 
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
scalePoly eff (IntPolyG cfg x coeffs) c = 
    let ?mixedMultInOutEffort = eff in
    IntPolyG cfg x $ map (\t -> t <*>| c) coeffs
scalePoly eff (IntPolyV cfg x polys) c = 
    IntPolyV cfg x $ map (\p -> scalePoly eff p c) polys

    
instance
    (ArithInOut.RoundedMixedDivideEffort cf other) => 
    ArithInOut.RoundedMixedDivideEffort (IntPoly var cf) other 
    where
    type ArithInOut.MixedDivEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedDivEffortIndicator cf other 
    mixedDivDefaultEffort (IntPolyG cfg _ _) c = 
        ArithInOut.mixedDivDefaultEffort (ipolycfg_sample_cf cfg) c
    mixedDivDefaultEffort (IntPolyV cfg _ _) c = 
        ArithInOut.mixedDivDefaultEffort (ipolycfg_sample_cf cfg) c
    
instance
    (ArithInOut.RoundedMixedDivide cf other) =>
    ArithInOut.RoundedMixedDivide (IntPoly var cf) other 
    where
    mixedDivInEff eff p1 a = error "inner rounded operations not available for IntPoly"
    mixedDivOutEff eff p1 a = divPolyByOther eff p1 a

divPolyByOther ::
    (ArithInOut.RoundedMixedDivide cf other) =>
    ArithInOut.MixedDivEffortIndicator cf other -> 
    IntPoly var cf -> other -> IntPoly var cf
divPolyByOther eff (IntPolyG cfg x coeffs) c = 
    let ?mixedDivInOutEffort = eff in
    IntPolyG cfg x $ map (\t -> t </>| c) coeffs
divPolyByOther eff (IntPolyV cfg x polys) c = 
    IntPolyV cfg x $ map (\p -> divPolyByOther eff p c) polys
    