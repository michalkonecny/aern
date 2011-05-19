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
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly (PolyPure)
import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (runST)
import Foreign.Storable

instance
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithUpDn.RoundedAddEffort (PolyPure cf) 
    where
    type ArithUpDn.AddEffortIndicator (PolyPure cf) = 
        ArithUpDn.RoundedRealEffortIndicator cf
    addDefaultEffort p =
        ArithUpDn.roundedRealDefaultEffort c
        where
        c =
            runST $
                do
                pM <- unsafeMakeMutable p
                cM <- Poly.peekConst pM
                unsafeReadMutable cM 

instance 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf)
    =>
    ArithUpDn.RoundedAddInPlace (PolyPure cf) 
    where
    addUpInPlaceEff effort resM p1M p2M =
        do
--        Poly.addUp opsPtr 
        undefined
            