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

import qualified Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly as Poly
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly (Poly)
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
    addUpInPlaceEff _ resM p1M p2M
        =
        do
        sampleM <- Poly.peekConstM p1M
        sample <- unsafeReadMutable sampleM
        polyAddUp sample resM p1M p2M 
    addDnInPlaceEff _ resM p1M p2M 
        =
        do
        sampleM <- Poly.peekConstM p1M
        sample <- unsafeReadMutable sampleM
        polyAddDn sample resM p1M p2M 
            