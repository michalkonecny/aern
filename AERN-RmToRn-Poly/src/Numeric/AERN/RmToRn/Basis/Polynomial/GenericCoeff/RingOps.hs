{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.RingOps
    Description :  in-place addition and multiplication of polynomials
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    In-place addition and multiplication of polynomials.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.RingOps 
()
where

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly (Poly)
-- import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Coeffs
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.RingOps

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (runST)
import Foreign.Storable

instance
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithUpDn.RoundedAddEffort (Poly cf) 
    where
    type ArithUpDn.AddEffortIndicator (Poly cf) = () 
    addDefaultEffort p = ()

instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithUpDn.RoundedAddInPlace (Poly cf)
    where
    addUpInPlaceEff _ = polyAddUp 
    addDnInPlaceEff _ = polyAddDn 

instance
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithInOut.RoundedAddEffort (Poly cf) 
    where
    type ArithInOut.AddEffortIndicator (Poly cf) = () 
    addDefaultEffort p = ()

instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithInOut.RoundedAddInPlace (Poly cf)
    where
    addOutInPlaceEff _ = polyAddEncl 
    addInInPlaceEff =
        error "attempting an inwards rounded addition for Poly cf, which is not supported" 
            
instance
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithUpDn.RoundedMultiplyEffort (Poly cf) 
    where
    type ArithUpDn.MultEffortIndicator (Poly cf) = () 
    multDefaultEffort p = ()

instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithUpDn.RoundedMultiplyInPlace (Poly cf)
    where
    multUpInPlaceEff _ = polyMultiplyUp 
    multDnInPlaceEff _ = polyMultiplyDn

instance
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithInOut.RoundedMultiplyEffort (Poly cf) 
    where
    type ArithInOut.MultEffortIndicator (Poly cf) = () 
    multDefaultEffort p = ()

instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithInOut.RoundedMultiplyInPlace (Poly cf)
    where
    multOutInPlaceEff _ = polyMultiplyEncl 
    multInInPlaceEff =
        error "attempting an inwards rounded multiplication for Poly cf, which is not supported" 
            
            