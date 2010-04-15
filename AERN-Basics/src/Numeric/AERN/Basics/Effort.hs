{-|
    Module      :  Numeric.AERN.Basics.Effort
    Description :  indicating computational effort for approximate computations
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Indicating computational effort for approximate computations.
-}
module Numeric.AERN.Basics.Effort where

class EffortIndicator t where
    {-| get an infinitely increasing sequence of effort indicators of the same type -}
    increments :: t -> [t]

instance EffortIndicator Int where
    increments i =
        map (i +) $ fibs12
        where
        fibs12 = scanl (+) 1 (1:fibs12)

instance (EffortIndicator t1, EffortIndicator t2) => EffortIndicator (t1, t2)
    where
    increments (i1, i2) =
        zip (increments i1) (increments i2)