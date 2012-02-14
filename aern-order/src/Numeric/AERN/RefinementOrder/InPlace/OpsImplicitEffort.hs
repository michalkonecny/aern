{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.RefinementOrder.InPlace.OpsImplicitEffort
    Description :  onvenience directed-rounded in-place lattice operations with implicit effort parameters 
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience directed-rounded in-place lattice operations with implicit effort parameters.
-}

module Numeric.AERN.RefinementOrder.InPlace.OpsImplicitEffort where

import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RefinementOrder

infixr 3 </\>=, <⊓>=, >/\<=, >⊓<= 
infixr 2 <\/>=, <⊔>=, >\/<=, >⊔<= 

-- | Outward rounded in-place join
joinOutInPlace :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable2 t s
joinOutInPlace = joinOutInPlaceEff ?joinmeetEffort 

-- | Outward rounded join assignment 
(<\/>=) :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable1 t s
(<\/>=) = mutable2ToMutable1 joinOutInPlace 

-- | Outward rounded in-place meet
meetOutInPlace :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable2 t s    
meetOutInPlace = meetOutInPlaceEff ?joinmeetEffort 

-- | Outward rounded meet assignment 
(</\>=) :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable1 t s    
(</\>=) = mutable2ToMutable1 meetOutInPlace 

-- | Inward rounded in-place join
joinInInPlace :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable2 t s
joinInInPlace = joinInInPlaceEff ?joinmeetEffort 

-- | Inward rounded join assignment 
(>\/<=) :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable1 t s
(>\/<=) = mutable2ToMutable1 joinInInPlace 

-- | Inward rounded in-place meet
meetInInPlace :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable2 t s    
meetInInPlace = meetInInPlaceEff ?joinmeetEffort 

-- | Inward rounded meet assignment 
(>/\<=) :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable1 t s    
(>/\<=) = mutable2ToMutable1 meetInInPlace 

{-| convenience Unicode notation for '<\/>=' -}
(<⊔>=) :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable1 t s
(<⊔>=) = (<\/>=)
{-| convenience Unicode notation for '</\>=' -}
(<⊓>=) :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable1 t s
(<⊓>=) = (</\>=)

{-| convenience Unicode notation for '>\/<=' -}
(>⊔<=) :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable1 t s
(>⊔<=) = (>\/<=)
{-| convenience Unicode notation for '>/\<=' -}
(>⊓<=) :: 
    (RoundedLatticeInPlace t, 
     ?joinmeetEffort :: JoinMeetEffortIndicator t) => 
    OpMutable1 t s
(>⊓<=) = (>/\<=)
