{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Conversion
    Description :  conversions between PolyFP and standard numeric types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Conversions between PolyFP and standard numeric types.

    This is a private module reexported publicly via its parent.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Conversion where

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Internal.Poly (PolyFP)

--import Numeric.AERN.RealArithmetic.NumericOrderRounding

--import Numeric.AERN.Basics.Exception
--import Control.Exception

import Foreign.Ptr(Ptr)
import System.IO.Unsafe (unsafePerformIO)

instance ArithUpDn.Convertible PolyFP Double where
    type ArithUpDn.ConvertEffortIndicator PolyFP Double = 
        Ptr Poly.Ops_Pure
    convertDefaultEffort _ _ =
        unsafePerformIO $ Poly.newOps Poly.Ops_Pure
    convertUpEff effort p =
       Just $ Poly.polyBoundUpThin effort p
    convertDnEff effort p =
       Just $ Poly.polyBoundDnThin effort p


