{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.Mutable.FieldOps
    Description :  field operations for mutable intervals 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Field operations for mutable intervals. 
    
    This module is hidden and reexported via its parent Interval.Mutable. 
-}

module Numeric.AERN.RealArithmetic.Interval.Mutable.FieldOps where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.Interval

import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.Interval.Mutable.ExactOps

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import Numeric.AERN.RealArithmetic.RefinementOrderRounding
import Numeric.AERN.RealArithmetic.Interval.FieldOps

instance (ArithUpDn.RoundedAddInPlace e, CanBeMutable e) => 
    RoundedAddInPlace (Interval e) 
    where
    addInInPlaceEff (Interval sample _) eff (MInterval resL resH) (MInterval aL aH) (MInterval bL bH) =
        do
        ArithUpDn.addUpInPlaceEff sample eff resL aL bL
        ArithUpDn.addDnInPlaceEff sample eff resH aH bH
    addOutInPlaceEff (Interval sample _) eff (MInterval resL resH) (MInterval aL aH) (MInterval bL bH) =
        do
        ArithUpDn.addDnInPlaceEff sample eff resL aL bL
        ArithUpDn.addUpInPlaceEff sample eff resH aH bH
    
instance (ArithUpDn.RoundedAddInPlace e, CanBeMutable e, NegInPlace e) => 
    RoundedSubtrInPlace (Interval e) 

instance (RoundedAbs (Interval e), CanBeMutable (Interval e)) => 
    RoundedAbsInPlace (Interval e) 

    