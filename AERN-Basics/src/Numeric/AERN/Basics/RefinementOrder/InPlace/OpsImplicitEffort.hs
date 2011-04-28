{-# LANGUAGE ImplicitParams #-}
{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.InPlace.OpsImplicitEffort
    Description :  onvenience directed-rounded in-place lattice operations with implicit effort parameters 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Convenience directed-rounded in-place lattice operations with implicit effort parameters.
-}

module Numeric.AERN.Basics.RefinementOrder.InPlace.OpsImplicitEffort where

import Numeric.AERN.Basics.Mutable
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import Numeric.AERN.Basics.RefinementOrder

infixr 3 </\>, <⊓>, >/\<, >⊓< 
infixr 2 <\/>, <⊔>, >\/<, >⊔< 

-- | Outward rounded in-place join
(<\/>) :: 
    (OuterRoundedLatticeInPlace t, 
     ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    OpMutable2 t s
(<\/>) = joinOutInPlaceEff ?joinmeetOutEffort 

-- | Outward rounded in-place meet
(</\>) :: 
    (OuterRoundedLatticeInPlace t, 
     ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    OpMutable2 t s    
(</\>) = meetOutInPlaceEff ?joinmeetOutEffort 

-- | Inward rounded in-place join
(>\/<) :: 
    (InnerRoundedLatticeInPlace t, 
     ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    OpMutable2 t s
(>\/<) = joinInInPlaceEff ?joinmeetInEffort 

-- | Inward rounded in-place meet
(>/\<) :: 
    (InnerRoundedLatticeInPlace t, 
     ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    OpMutable2 t s    
(>/\<) = meetInInPlaceEff ?joinmeetInEffort 

{-| convenience Unicode notation for '<\/>' -}
(<⊔>) :: 
    (OuterRoundedLatticeInPlace t, ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    OpMutable2 t s
(<⊔>) = (<\/>)
{-| convenience Unicode notation for '</\>' -}
(<⊓>) :: 
    (OuterRoundedLatticeInPlace t, ?joinmeetOutEffort :: JoinMeetOutEffortIndicator t) => 
    OpMutable2 t s
(<⊓>) = (</\>)

{-| convenience Unicode notation for '>\/<' -}
(>⊔<) :: 
    (InnerRoundedLatticeInPlace t, ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    OpMutable2 t s
(>⊔<) = (>\/<)
{-| convenience Unicode notation for '>/\<' -}
(>⊓<) :: 
    (InnerRoundedLatticeInPlace t, ?joinmeetInEffort :: JoinMeetInEffortIndicator t) => 
    OpMutable2 t s
(>⊓<) = (>/\<)
