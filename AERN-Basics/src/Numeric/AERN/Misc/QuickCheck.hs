{-|
    Module      :  Numeric.AERN.Misc.QuickCheck
    Description :  miscellaneous utilities for QuickCheck  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    miscellaneous utilities for QuickCheck
-}
module Numeric.AERN.Misc.QuickCheck where

import Test.QuickCheck

{-| Run the generator with size increased by 1 (useful for avoiding 
    too narrow selection at size 0 - in particular Double "randomly" generates 
    0 with probability 1 at size 0. 
-}
incrSize :: Gen t -> Gen t
incrSize gen = sized (\size -> resize (size + 1) gen)

{-|
    Probability of True is @n/m@.  Precondition: @0<n<m@    
-}
arbitraryBoolRatio :: Int {-^ @n@ -} -> Int {-^ @m@ -} -> Gen Bool 
arbitraryBoolRatio n m | 0 < n && n < m =
    frequency [(n, return True), (m - n, return False)]
