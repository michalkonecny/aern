{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Conversion
    Description :  conversions between PolyFP and standard numeric types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Conversions between Poly and standard numeric types.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Conversion 
()
where

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly (Poly)
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (runST)
import Foreign.Storable

-- bounds as conversion to coeff type:
instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithUpDn.Convertible (Poly cf) cf 
    where
    type ArithUpDn.ConvertEffortIndicator (Poly cf) cf = 
        () -- OpsFP cf
    convertDefaultEffort sampleP sampleCF =
        () -- Poly.opsFPArithUpDnDefaultEffort sampleCF
    convertUpEff effort p =
        runST $
            do
            pM <- unsafeMakeMutable p
            resM <- unsafeMakeMutable zero
            polyBoundUp resM pM
            res <- unsafeReadMutable resM
            return $ Just $ res
    convertDnEff effort p =
        runST $
            do
            pM <- unsafeMakeMutable p
            resM <- unsafeMakeMutable zero
            polyBoundDn resM pM
            res <- unsafeReadMutable resM
            return $ Just $ res
            