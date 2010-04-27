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

import Test.QuickCheck

class EffortIndicator t where
    {-| get a range of independent increments to a given effort indicator (may be empty) -}
    effortIncrementVariants :: t -> [t]
    {-| repeat the increment present in the given pair for the larger effort indicator -}
    effortRepeatIncrement :: (t,t) -> t
    {-| get an infinitely increasing sequence of effort indicators of the same type -}
    effortIncrementSequence :: t -> [t]

newtype Int1To100 = Int1To100 { fromInt1To100 :: Int }
newtype Int1To1000 = Int1To1000 { fromInt1To1000 :: Int }

instance Show Int1To100 where
    show (Int1To100 i) = show i

instance Show Int1To1000 where
    show (Int1To1000 i) = show i

instance Arbitrary Int1To100 where
    arbitrary =
        do
        i <- choose (1, 100)
        return $ Int1To100 i

instance Arbitrary Int1To1000 where
    arbitrary =
        do
        i <- choose (1, 1000)
        return $ Int1To1000 i

instance EffortIndicator Int1To100 where
    effortIncrementVariants (Int1To100 i) = [Int1To100 $ i + 1]
    effortRepeatIncrement (Int1To100 i1, Int1To100 i2) = Int1To100 $ i2 + (i2 - i1)
    effortIncrementSequence (Int1To100 i) =
        map Int1To100 $ map (i +) $ fibs12
        where
        fibs12 = scanl (+) 1 (1:fibs12)

instance EffortIndicator Int1To1000 where
    effortIncrementVariants (Int1To1000 i) = [Int1To1000 $ i + 1]
    effortRepeatIncrement (Int1To1000 i1, Int1To1000 i2) = Int1To1000 $ i2 + (i2 - i1)
    effortIncrementSequence (Int1To1000 i) =
        map Int1To1000 $ map (i +) $ fibs12
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
        
        