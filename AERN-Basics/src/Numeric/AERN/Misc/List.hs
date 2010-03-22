{-|
    Module      :  Numeric.AERN.Misc.List
    Description :  miscellaneous list functions  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Miscellaneous list functions.
-}
module Numeric.AERN.Misc.List where

import qualified Data.List as List

sortUsing :: (Ord b) => (a -> b) -> [a] -> [a]
sortUsing f =
    List.sortBy compareF
    where
    compareF a b = compare (f a) (f b)
