{-|
    Module      :  Numeric.AERN.RefinementOrder.InPlace.OpsDefaultEffort
    Description :  Convenience directed-rounded in-place lattice operations with default effort parameters 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience directed-rounded in-place lattice operations with default effort parameters.
-}

module Numeric.AERN.RefinementOrder.InPlace.OpsDefaultEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RefinementOrder

infixr 3 </\>=, >/\<=, <⊓>=, >⊓<= 
infixr 2 <\/>=, >\/<=, <⊔>=, >⊔<= 

-- | Outward rounded in-place meet
meetOutInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
meetOutInPlace = mutable2EffToMutable2 meetOutInPlaceEff joinmeetDefaultEffort 

-- | Outward rounded meet assignment
(</\>=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(</\>=) = mutable2ToMutable1 meetOutInPlace

-- | Inward rounded in-place meet
meetInInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
meetInInPlace = mutable2EffToMutable2 meetInInPlaceEff joinmeetDefaultEffort 

-- | Inward rounded meet assignment
(>/\<=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(>/\<=) = mutable2ToMutable1 meetInInPlace

-- | Outward rounded in-place join
joinOutInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
joinOutInPlace = mutable2EffToMutable2 joinOutInPlaceEff joinmeetDefaultEffort 

-- | Outward rounded join assignment
(<\/>=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(<\/>=) = mutable2ToMutable1 joinOutInPlace

-- | Inward rounded in-place join
joinInInPlace :: (RoundedLatticeInPlace t) => OpMutable2 t s
joinInInPlace = mutable2EffToMutable2 joinInInPlaceEff joinmeetDefaultEffort 

-- | Inward rounded join assignment
(>\/<=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(>\/<=) = mutable2ToMutable1 joinInInPlace

-- | Partial outward rounded in-place join
partialJoinOutInPlace :: (RoundedBasisInPlace t) => OpPartialMutable2 t s
partialJoinOutInPlace = 
    partialMutable2EffToPartialMutable2 partialJoinOutInPlaceEff partialJoinDefaultEffort 

-- | Partial inward rounded in-place join
partialJoinInInPlace :: (RoundedBasisInPlace t) => OpPartialMutable2 t s
partialJoinInInPlace = 
    partialMutable2EffToPartialMutable2 partialJoinInInPlaceEff partialJoinDefaultEffort

{-| Convenience Unicode notation for '<\/>=' -}
(<⊔>=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(<⊔>=) = (<\/>=)

{-| Convenience Unicode notation for '</\>=' -}
(<⊓>=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(<⊓>=) = (</\>=)

{-| Convenience Unicode notation for '>\/<=' -}
(>⊔<=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(>⊔<=) = (>\/<=)

{-| Convenience Unicode notation for '>/\<=' -}
(>⊓<=) :: (RoundedLatticeInPlace t) => OpMutable1 t s
(>⊓<=) = (>/\<=)
