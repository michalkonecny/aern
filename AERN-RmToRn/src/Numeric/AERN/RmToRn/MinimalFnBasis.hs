{-|
    Module      :  Numeric.AERN.RmToRn.MinimalFnBasis
    Description :  minimal ADT for function interval endpoints  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Minimal ADT for function interval endpoints.
    
    This module should be only used by libraries that provide
    a function interval endpoint.
-}

module Numeric.AERN.RmToRn.MinimalFnBasis 
    (
        module Numeric.AERN.RmToRn.MinimalFnBasis.Basics,
        module Numeric.AERN.RmToRn.MinimalFnBasis.NumericOrder,
        module Numeric.AERN.RmToRn.MinimalFnBasis.RingOps
    )
where

import Numeric.AERN.RmToRn.MinimalFnBasis.Basics
import Numeric.AERN.RmToRn.MinimalFnBasis.NumericOrder --()
import Numeric.AERN.RmToRn.MinimalFnBasis.RingOps --()
