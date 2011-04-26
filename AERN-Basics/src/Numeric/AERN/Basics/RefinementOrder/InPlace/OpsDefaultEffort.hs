{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.InPlace.OpsDefaultEffort
    Description :  Convenience directed-rounded in-place lattice operations with default effort parameters 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience directed-rounded in-place lattice operations with default effort parameters.
-}

module Numeric.AERN.Basics.RefinementOrder.InPlace.OpsDefaultEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.RefinementOrder

infixr 3 </\>=, >/\<=, <⊓>=, >⊓<= 
infixr 2 <\/>=, >\/<=, <⊔>=, >⊔<= 

-- | Outward rounded in-place meet
meetOutInPlace :: (OuterRoundedLatticeInPlace t) => OpMutable2 t s
meetOutInPlace = mutable2EffToMutable2 meetOutInPlaceEff joinmeetOutDefaultEffort 

-- | Outward rounded meet assignment
(</\>=) :: (OuterRoundedLatticeInPlace t) => OpMutable1 t s
(</\>=) = mutable2ToMutable1 meetOutInPlace

-- | Inward rounded in-place meet
meetInInPlace :: (InnerRoundedLatticeInPlace t) => OpMutable2 t s
meetInInPlace = mutable2EffToMutable2 meetInInPlaceEff joinmeetInDefaultEffort 

-- | Inward rounded meet assignment
(>/\<=) :: (InnerRoundedLatticeInPlace t) => OpMutable1 t s
(>/\<=) = mutable2ToMutable1 meetInInPlace

-- | Outward rounded in-place join
joinOutInPlace :: (OuterRoundedLatticeInPlace t) => OpMutable2 t s
joinOutInPlace = mutable2EffToMutable2 joinOutInPlaceEff joinmeetOutDefaultEffort 

-- | Outward rounded join assignment
(<\/>=) :: (OuterRoundedLatticeInPlace t) => OpMutable1 t s
(<\/>=) = mutable2ToMutable1 joinOutInPlace

-- | Inward rounded in-place join
joinInInPlace :: (InnerRoundedLatticeInPlace t) => OpMutable2 t s
joinInInPlace = mutable2EffToMutable2 joinInInPlaceEff joinmeetInDefaultEffort 

-- | Inward rounded join assignment
(>\/<=) :: (InnerRoundedLatticeInPlace t) => OpMutable1 t s
(>\/<=) = mutable2ToMutable1 joinInInPlace

{-| Convenience Unicode notation for '<\/>=' -}
(<⊔>=) :: (OuterRoundedLatticeInPlace t) => OpMutable1 t s
(<⊔>=) = (<\/>=)

{-| Convenience Unicode notation for '</\>=' -}
(<⊓>=) :: (OuterRoundedLatticeInPlace t) => OpMutable1 t s
(<⊓>=) = (</\>=)

{-| Convenience Unicode notation for '>\/<=' -}
(>⊔<=) :: (InnerRoundedLatticeInPlace t) => OpMutable1 t s
(>⊔<=) = (>\/<=)

{-| Convenience Unicode notation for '>/\<=' -}
(>⊓<=) :: (InnerRoundedLatticeInPlace t) => OpMutable1 t s
(>⊓<=) = (>/\<=)
