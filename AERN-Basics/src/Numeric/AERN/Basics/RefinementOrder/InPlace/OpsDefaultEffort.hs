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
meetOut :: (CanBeMutable t, OuterRoundedLattice t) => OpMutable2 t s
meetOut = pureEffToMutable2 meetOutEff joinmeetOutDefaultEffort 

-- | Outward rounded meet assignment
(</\>=) :: (CanBeMutable t, OuterRoundedLattice t) => OpMutable1 t s
(</\>=) = mutable2ToMutable1 meetOut

-- | Inward rounded in-place meet
meetIn :: (CanBeMutable t, InnerRoundedLattice t) => OpMutable2 t s
meetIn = pureEffToMutable2 meetInEff joinmeetInDefaultEffort 

-- | Inward rounded meet assignment
(>/\<=) :: (CanBeMutable t, InnerRoundedLattice t) => OpMutable1 t s
(>/\<=) = mutable2ToMutable1 meetIn 

-- | Outward rounded in-place join
joinOut :: (CanBeMutable t, OuterRoundedLattice t) => OpMutable2 t s
joinOut = pureEffToMutable2 joinOutEff joinmeetOutDefaultEffort 

-- | Outward rounded join assignment
(<\/>=) :: (CanBeMutable t, OuterRoundedLattice t) => OpMutable1 t s
(<\/>=) = mutable2ToMutable1 joinOut 

-- | Inward rounded in-place join
joinIn :: (CanBeMutable t, InnerRoundedLattice t) => OpMutable2 t s
joinIn = pureEffToMutable2 joinInEff joinmeetInDefaultEffort 

-- | Inward rounded join assignment
(>\/<=) :: (CanBeMutable t, InnerRoundedLattice t) => OpMutable1 t s
(>\/<=) = mutable2ToMutable1 joinIn 

{-| Convenience Unicode notation for '<\/>=' -}
(<⊔>=) :: (CanBeMutable t, OuterRoundedLattice t) => OpMutable1 t s
(<⊔>=) = (<\/>=)

{-| Convenience Unicode notation for '</\>=' -}
(<⊓>=) :: (CanBeMutable t, OuterRoundedLattice t) => OpMutable1 t s
(<⊓>=) = (</\>=)

{-| Convenience Unicode notation for '>\/<=' -}
(>⊔<=) :: (CanBeMutable t, InnerRoundedLattice t) => OpMutable1 t s
(>⊔<=) = (>\/<=)

{-| Convenience Unicode notation for '>/\<=' -}
(>⊓<=) :: (CanBeMutable t, InnerRoundedLattice t) => OpMutable1 t s
(>⊓<=) = (>/\<=)
