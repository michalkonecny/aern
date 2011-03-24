{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Interval.SpecialConst
    Description :  common constants such as pi
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Common constants such as pi.
-}

module Numeric.AERN.RealArithmetic.Interval.SpecialConst where

import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut

import Numeric.AERN.Basics.Interval

instance (ArithUpDn.RoundedSpecialConstEffort e) => 
    (ArithInOut.RoundedSpecialConstEffort (Interval e)) 
    where
    type ArithInOut.SpecialConstEffortIndicator (Interval e) = 
        ArithUpDn.SpecialConstEffortIndicator e
    specialConstDefaultEffort (Interval l h) = 
        ArithUpDn.specialConstDefaultEffort l

instance (ArithUpDn.RoundedSpecialConst e) => 
    (ArithInOut.RoundedSpecialConst (Interval e)) 
    where
    piInEff effort =
        Interval (ArithUpDn.piUpEff effort) (ArithUpDn.piDnEff effort)
    piOutEff effort =
        Interval (ArithUpDn.piDnEff effort) (ArithUpDn.piUpEff effort)
    eInEff effort =
        Interval (ArithUpDn.eUpEff effort) (ArithUpDn.eDnEff effort)
    eOutEff effort =
        Interval (ArithUpDn.eDnEff effort) (ArithUpDn.eUpEff effort)

