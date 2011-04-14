
module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.OpsDefaultEffort where

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.FieldOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding

addOutInPlace d = addOutInPlaceEff d (addDefaultEffort d) 
