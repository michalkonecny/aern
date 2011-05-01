{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.InPlace.OpsImplicitEffort
    Description :  onvenience directed-rounded in-place lattice operations with implicit effort parameters 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience directed-rounded in-place lattice operations with implicit effort parameters.
-}

module Numeric.AERN.Basics.RefinementOrder.InPlace.OpsImplicitEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.Basics.RefinementOrder

infixr 3 </\>=, <⊓>=, >/\<=, >⊓<= 
infixr 2 <\/>=, <⊔>=, >\/<=, >⊔<= 

-- | Outward rounded in-place join
joinOutInPlace :: 
    (OuterRoundedLatticeInPlace t, 
     ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    OpMutable2 t s
joinOutInPlace = joinOutInPlaceEff ?joinmeetOutEffort 

-- | Outward rounded join assignment 
(<\/>=) :: 
    (OuterRoundedLatticeInPlace t, 
     ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    OpMutable1 t s
(<\/>=) = mutable2ToMutable1 joinOutInPlace 

-- | Outward rounded in-place meet
meetOutInPlace :: 
    (OuterRoundedLatticeInPlace t, 
     ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    OpMutable2 t s    
meetOutInPlace = meetOutInPlaceEff ?joinmeetOutEffort 

-- | Outward rounded meet assignment 
(</\>=) :: 
    (OuterRoundedLatticeInPlace t, 
     ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    OpMutable1 t s    
(</\>=) = mutable2ToMutable1 meetOutInPlace 

-- | Inward rounded in-place join
joinInInPlace :: 
    (InnerRoundedLatticeInPlace t, 
     ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    OpMutable2 t s
joinInInPlace = joinInInPlaceEff ?joinmeetInEffort 

-- | Inward rounded join assignment 
(>\/<=) :: 
    (InnerRoundedLatticeInPlace t, 
     ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    OpMutable1 t s
(>\/<=) = mutable2ToMutable1 joinInInPlace 

-- | Inward rounded in-place meet
meetInInPlace :: 
    (InnerRoundedLatticeInPlace t, 
     ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    OpMutable2 t s    
meetInInPlace = meetInInPlaceEff ?joinmeetInEffort 

-- | Inward rounded meet assignment 
(>/\<=) :: 
    (InnerRoundedLatticeInPlace t, 
     ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    OpMutable1 t s    
(>/\<=) = mutable2ToMutable1 meetInInPlace 

{-| convenience Unicode notation for '<\/>=' -}
(<⊔>=) :: 
    (OuterRoundedLatticeInPlace t, 
     ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    OpMutable1 t s
(<⊔>=) = (<\/>=)
{-| convenience Unicode notation for '</\>=' -}
(<⊓>=) :: 
    (OuterRoundedLatticeInPlace t, 
     ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    OpMutable1 t s
(<⊓>=) = (</\>=)

{-| convenience Unicode notation for '>\/<=' -}
(>⊔<=) :: 
    (InnerRoundedLatticeInPlace t, 
     ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    OpMutable1 t s
(>⊔<=) = (>\/<=)
{-| convenience Unicode notation for '>/\<=' -}
(>⊓<=) :: 
    (InnerRoundedLatticeInPlace t, 
     ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    OpMutable1 t s
(>⊓<=) = (>/\<=)
