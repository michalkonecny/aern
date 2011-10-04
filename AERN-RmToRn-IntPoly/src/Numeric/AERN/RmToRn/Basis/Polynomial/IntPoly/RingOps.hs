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

import Numeric.AERN.RmToRn.New

import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.OpsImplicitEffort
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Auxiliary

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
        (ArithInOut.MultEffortIndicator cf, ArithInOut.AddEffortIndicator cf) 
    multDefaultEffort (IntPoly cfg _) = 
        (ArithInOut.multDefaultEffort sample_cf, ArithInOut.addDefaultEffort sample_cf) 
        where
        sample_cf = (ipolycfg_sample_cf cfg)
    
instance
    (ArithInOut.RoundedReal cf, Show var, Show cf) =>
    ArithInOut.RoundedMultiply (IntPoly var cf) 
    where
    multInEff eff p1 p2 = error "inner rounded operations not available for IntPoly"
    multOutEff eff p1 p2 = multPolys eff p1 p2
    
multPolys ::
    (ArithInOut.RoundedReal cf, Show var, Show cf) =>
    (ArithInOut.MultEffortIndicator cf, ArithInOut.AddEffortIndicator cf) -> 
    IntPoly var cf -> IntPoly var cf -> IntPoly var cf
multPolys eff@(effMult, effAdd) (IntPoly cfg1 poly1) (IntPoly cfg2 poly2)
    =
    let ?addInOutEffort = effAdd in
    let ?multInOutEffort = effMult in
    (IntPoly cfg1 $ termsNormalise cfg1 $ multTerms poly1 poly2)

multTerms poly1@(IntPolyC val1) poly2@(IntPolyC val2) = IntPolyC $ val1 <*> val2 
multTerms poly1@(IntPolyV xName1 polys1) poly2@(IntPolyV xName2 polys2)
    =
    IntPolyV xName2 multPolys
    where
    multPolys 
        = IntMap.fromListWith addTerms $
            [(n1 + n2, multTerms p1 p2) | 
                (n1, p1) <- IntMap.toAscList polys1, 
                (n2, p2) <- IntMap.toAscList polys2 ] 
        
powTerms vars = powerFromMult (mkConstTerms one vars) multTerms

        
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
    IntPoly cfg $ addP poly
    where
    addP(IntPolyC val) =
        IntPolyC $ val <+>| const
    addP (IntPolyV x polys) =
        IntPolyV x $ IntMap.insert 0 newConstPoly polys
        where
        oldConstPoly =
            case IntMap.lookup 0 polys of
                Nothing -> intpoly_terms $ newConstFn cfgR undefined zero
                Just p -> p
        newConstPoly = addP oldConstPoly
        cfgR = cfgRemVar cfg
        domsR = ipolycfg_doms cfgR
         
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
    sP (IntPolyC val) =
        IntPolyC $ val <*>| c
    sP (IntPolyV x polys) = 
        IntPolyV x $ IntMap.map sP polys

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
    