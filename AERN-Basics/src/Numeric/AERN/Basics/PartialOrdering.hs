{-|
    Module      :  Numeric.AERN.Basics.PartialOrdering
    Description :  extension of Prelude.Ordering with non-comparable variant  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Extension of 'Prelude.Ordering' with non-comparable variant.
-}
module Numeric.AERN.Basics.PartialOrdering where

import qualified Prelude
import Test.QuickCheck

{-| Like 'Prelude.Ordering' but with a non-comparable option -}
data PartialOrdering = EQ | LT | GT | NC
    deriving (Prelude.Eq)
    
toPartialOrdering :: Prelude.Ordering -> PartialOrdering
toPartialOrdering Prelude.EQ = EQ 
toPartialOrdering Prelude.LT = LT 
toPartialOrdering Prelude.GT = GT 

{-| flip an ordering relation -}
partialOrderingTranspose :: PartialOrdering -> PartialOrdering
partialOrderingTranspose LT = GT
partialOrderingTranspose GT = LT
partialOrderingTranspose a = a

{-|
    Poset with the ability to randomly generate
    pairs of its own elements that are in 
    a specific order relation (eg LT or NC).
    
    This is to help with checking properties that
    make sense only for pairs in a certain relation
    where such pairs are rare.
-}
class ArbitraryOrderedPair t where
    {-| generator of pairs that satisfy the chosen relation -}
    arbitraryPairRelatedBy :: PartialOrdering -> Gen (t,t)    
    {-| generator of pairs distributed in such a way that all ordering relations 
       permitted by this structure have similar probabilities of occurrence -}
    arbitraryPairUniformRelation :: Gen (t,t)

