{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.SpecialConst
    Description :  support for common constants such as pi
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Support for common constants such as pi.
    
    This module is hidden and reexported via its parent RefinementOrderRounding. 
-}

module Numeric.AERN.RealArithmetic.RefinementOrderRounding.SpecialConst where

--import Numeric.AERN.Basics.Effort
--import Numeric.AERN.Basics.Exception
--import Numeric.AERN.Basics.ShowInternals
--import Numeric.AERN.RealArithmetic.Laws
--import Numeric.AERN.RealArithmetic.Measures
--import qualified Numeric.AERN.NumericOrder as NumOrd
--import Numeric.AERN.NumericOrder.OpsImplicitEffort
--
--import Numeric.AERN.Misc.Debug
--
--import Test.QuickCheck
--import Test.Framework (testGroup, Test)
--import Test.Framework.Providers.QuickCheck2 (testProperty)

class RoundedSpecialConstEffort t where
    type SpecialConstEffortIndicator t
    specialConstDefaultEffort :: t -> SpecialConstEffortIndicator t

class (RoundedSpecialConstEffort t) => RoundedSpecialConst t where
    piInEff :: (SpecialConstEffortIndicator t) -> t
    piOutEff :: (SpecialConstEffortIndicator t) -> t
    eInEff :: (SpecialConstEffortIndicator t) -> t
    eOutEff :: (SpecialConstEffortIndicator t) -> t


