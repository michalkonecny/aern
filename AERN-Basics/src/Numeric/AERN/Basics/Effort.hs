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
    {-| get a range of independent increments to a given effort indicator (may be empty) -}
    effortIncrementVariants :: t -> [t]
    {-| repeat the increment present in the given pair for the larger effort indicator -}
    effortRepeatIncrement :: (t,t) -> t
    {-| get an infinitely increasing sequence of effort indicators of the same type -}
    effortIncrementSequence :: t -> [t]

instance EffortIndicator Int where
    effortIncrementVariants i = [i + 1]
    effortRepeatIncrement (i1,i2) = i2 + (i2 - i1)
    effortIncrementSequence i =
        map (i +) $ fibs12
        where
        fibs12 = scanl (+) 1 (1:fibs12)

instance EffortIndicator () where
    effortIncrementVariants _ = []
    effortRepeatIncrement _ = ()
    effortIncrementSequence i = repeat ()

instance (EffortIndicator t1, EffortIndicator t2) => EffortIndicator (t1, t2)
    where
    effortIncrementVariants (i1, i2) = 
        i1Variants ++ i2Variants
        where
        i1Variants =
            map (\i -> (i, i2)) (effortIncrementVariants i1)
        i2Variants =
            map (\i -> (i1, i)) (effortIncrementVariants i2)
    effortRepeatIncrement ((i1, i2), (j1, j2)) = 
        (effortRepeatIncrement (i1, j1), effortRepeatIncrement (i2, j2)) 
    effortIncrementSequence (i1, i2) =
        zip (effortIncrementSequence i1) (effortIncrementSequence i2)
        
        