{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.Basis.Double.MixedFieldOps
    Description :  rounded basic arithmetic operations mixing 2 types
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Rounded basic arithmetical operations mixing 2 types.
    
    This module is hidden and reexported via its parent NumericOrderRounding. 
-}

module Numeric.AERN.RealArithmetic.Basis.Double.MixedFieldOps where

import Numeric.AERN.RealArithmetic.Basis.Double.Conversion
import Numeric.AERN.RealArithmetic.Basis.Double.NumericOrder
import Numeric.AERN.RealArithmetic.Basis.Double.FieldOps

import Numeric.AERN.RealArithmetic.NumericOrderRounding

instance RoundedMixedAdd Integer Double
    where
    type MixedAddEffortIndicator Integer Double =
        MixedAddEffortIndicatorByConversion Integer Double 
    mixedAddDefaultEffort = mixedAddDefaultEffortByConversion 
    mixedAddUpEff = mixedAddUpEffByConversion
    mixedAddDnEff = mixedAddDnEffByConversion

instance RoundedMixedMultiply Integer Double
    where
    type MixedMultEffortIndicator Integer Double =
        MixedMultEffortIndicatorByConversion Integer Double 
    mixedMultDefaultEffort = mixedMultDefaultEffortByConversion 
    mixedMultUpEff = mixedMultUpEffByConversion
    mixedMultDnEff = mixedMultDnEffByConversion

instance RoundedMixedDivide Integer Double
    where
    type MixedDivEffortIndicator Integer Double =
        MixedDivEffortIndicatorByConversion Integer Double 
    mixedDivDefaultEffort = mixedDivDefaultEffortByConversion 
    mixedDivUpEff = mixedDivUpEffByConversion
    mixedDivDnEff = mixedDivDnEffByConversion

