{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.NumericOrderRounding.SpecialConst
    Description :  support for common constants such as pi
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Support for common constants such as pi.
    
    This module is hidden and reexported via its parent NumericOrderRounding. 
-}

module Numeric.AERN.RealArithmetic.NumericOrderRounding.SpecialConst where

import Numeric.AERN.Basics.Effort

class
    (EffortIndicator (SpecialConstEffortIndicator t))
    => 
    RoundedSpecialConstEffort t 
    where
    type SpecialConstEffortIndicator t
    specialConstDefaultEffort :: t -> SpecialConstEffortIndicator t

class (RoundedSpecialConstEffort t) => RoundedSpecialConst t where
    piUpEff :: (SpecialConstEffortIndicator t) -> t
    piDnEff :: (SpecialConstEffortIndicator t) -> t
    eUpEff :: (SpecialConstEffortIndicator t) -> t
    eDnEff :: (SpecialConstEffortIndicator t) -> t

piUp :: (RoundedSpecialConst t) => t -> t
piUp sample = piUpEff (specialConstDefaultEffort sample)

piDn :: (RoundedSpecialConst t) => t -> t
piDn sample = piDnEff (specialConstDefaultEffort sample)

eUp :: (RoundedSpecialConst t) => t -> t
eUp sample = eUpEff (specialConstDefaultEffort sample)

eDn :: (RoundedSpecialConst t) => t -> t
eDn sample = eDnEff (specialConstDefaultEffort sample)

