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

    Conversions between PolyPure and standard numeric types.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Conversion 
()
where

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly (PolyPure)
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (runST)
import Foreign.Storable

instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithUpDn.Convertible (PolyPure cf) cf 
    where
    type ArithUpDn.ConvertEffortIndicator (PolyPure cf) cf = 
        ArithUpDn.RoundedRealEffortIndicator cf
    convertDefaultEffort sampleP sampleCF =
        ArithUpDn.roundedRealDefaultEffort sampleCF 
    convertUpEff effort p =
        runST $
            do
            pM <- unsafeMakeMutable p
            sampleM <- Poly.peekConst pM
            sample <- unsafeReadMutable sampleM 
            let opsPtr = Poly.newOpsMutableArithUpDn sample effort
            resM <- cloneMutable sampleM
            polyBoundUpThin opsPtr resM pM
            res <- unsafeReadMutable resM
            return $ Just $ res
    convertDnEff effort p =
        runST $
            do
            pM <- unsafeMakeMutable p
            sampleM <- Poly.peekConst pM
            sample <- unsafeReadMutable sampleM 
            let opsPtr = Poly.newOpsMutableArithUpDn sample effort
            resM <- cloneMutable sampleM
            polyBoundDnThin opsPtr resM pM
            res <- unsafeReadMutable resM
            return $ Just $ res
