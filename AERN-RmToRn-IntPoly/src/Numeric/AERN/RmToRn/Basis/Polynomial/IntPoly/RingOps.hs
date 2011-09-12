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
    addDefaultEffort (IntPoly cfg _) = ArithInOut.addDefaultEffort (ipolycfg_sample_cf cfg)
    
instance
    (ArithInOut.RoundedAdd cf, Show var, Show cf) =>
    ArithInOut.RoundedAdd (IntPoly var cf) 
    where
    addInEff eff p1 p2 = error "inner rounded operations not available for IntPoly"
    addOutEff eff p1 p2 = addPolys eff p1 p2
    
addPolys :: 
    (ArithInOut.RoundedAdd cf, Show var, Show cf) =>
    ArithInOut.AddEffortIndicator cf -> 
    IntPoly var cf -> IntPoly var cf -> IntPoly var cf
addPolys eff (IntPoly cfg1 poly1) (IntPoly cfg2 poly2)
    =
    let ?addInOutEffort = eff in
    (IntPoly cfg1 $ addP poly1 poly2)
    where
    addP poly1@(IntPolyG xName1 coeffs1) poly2@(IntPolyG xName2 coeffs2) 
        | lengthPoly1 == lengthPoly2 =
            IntPolyG xName1 $ zipWith (<+>) coeffs1 coeffs2
        | lengthPoly1 > lengthPoly2 =
            IntPolyG xName1 $ highCoeffs1 ++ (zipWith (<+>) lowCoeffs1  coeffs2)
        | otherwise =
            IntPolyG xName1 $ highCoeffs2 ++ (zipWith (<+>) lowCoeffs2  coeffs1)
        where
        (highCoeffs2, lowCoeffs2) = splitAt (lengthPoly2 - lengthPoly1) coeffs2
        (highCoeffs1, lowCoeffs1) = splitAt (lengthPoly1 - lengthPoly2) coeffs1
        lengthPoly1 = length coeffs1
        lengthPoly2 = length coeffs2
    addP poly1@(IntPolyV xName1 polys1) poly2@(IntPolyV xName2 polys2) 
        | lengthPoly1 == lengthPoly2 =
            IntPolyV xName1 $ zipWith addP polys1 polys2
        | lengthPoly1 > lengthPoly2 =
            IntPolyV xName1 $ highPolys1 ++ (zipWith addP lowPolys1  polys2)
        | otherwise =
            IntPolyV xName1 $ highPolys2 ++ (zipWith addP lowPolys2  polys1)
        where
        (highPolys2, lowPolys2) = splitAt (lengthPoly2 - lengthPoly1) polys2
        (highPolys1, lowPolys1) = splitAt (lengthPoly1 - lengthPoly2) polys1
        lengthPoly1 = length polys1
        lengthPoly2 = length polys2
    addP p1 p2 =
        error $ "addPolys: cannot add p1=" ++ show p1 ++ " and p2=" ++ show p2
    
instance
    (ArithInOut.RoundedMixedAddEffort cf other) => 
    ArithInOut.RoundedMixedAddEffort (IntPoly var cf) other 
    where
    type ArithInOut.MixedAddEffortIndicator (IntPoly var cf) other = ArithInOut.MixedAddEffortIndicator cf other  
    mixedAddDefaultEffort (IntPoly cfg _) c = 
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
addPolyConst eff (IntPoly cfg poly) const =
    let ?mixedAddInOutEffort = eff in
    IntPoly cfg $ addP poly
    where
    addP(IntPolyG x []) =
        IntPolyG x [zero <+>| const]
    addP (IntPolyG x coeffs) =
        IntPolyG x $ aux coeffs
        where
        aux [c] = [c <+>| const]
        aux (t : ts) = t : (aux ts)  
    addP (IntPolyV x polys) =
        IntPolyV x $ aux polys
        where
        aux [p] = [addP p]
        aux (t : ts) = t : (aux ts)  
    
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
    IntPoly cfg $ sP poly 
    where
    sP (IntPolyG x coeffs) =
        IntPolyG x $ map (\t -> t <*>| c) coeffs
    sP (IntPolyV x polys) = 
        IntPolyV x $ map sP polys

instance
    (ArithInOut.RoundedMixedDivideEffort cf other) => 
    ArithInOut.RoundedMixedDivideEffort (IntPoly var cf) other 
    where
    type ArithInOut.MixedDivEffortIndicator (IntPoly var cf) other = 
        ArithInOut.MixedDivEffortIndicator cf other 
    mixedDivDefaultEffort (IntPoly cfg _) c = 
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
divPolyByOther eff (IntPoly cfg poly) c = 
    let ?mixedDivInOutEffort = eff in
    IntPoly cfg $ dP poly 
    where
    dP (IntPolyG x coeffs) =
        IntPolyG x $ map (\t -> t </>| c) coeffs
    dp (IntPolyV x polys) = 
        IntPolyV x $ map dp polys
    