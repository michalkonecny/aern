{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.RoundedLattice
    Description :  lattices with outwards and inwards rounded operations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}
module Numeric.AERN.Basics.RefinementOrder.RoundedLattice 
where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.MaybeBool
import Numeric.AERN.Basics.Equality
import Numeric.AERN.Basics.PartialOrdering
--import Numeric.AERN.Basics.RefinementOrder.Extrema
--import Numeric.AERN.Basics.Laws.SemidecidableRelation

{-|
    A type with outward-rounding lattice operations.
-}
class OuterRoundedLattice t where
    joinOut :: [EffortIndicator] -> t -> t -> t
    meetOut :: [EffortIndicator] -> t -> t -> t
    joinmeetOutDefaultEffort :: t -> [EffortIndicator]

    (<|\/>) :: t -> t -> t
    (<|/\>) :: t -> t -> t
    
    a <|\/> b = joinOut (joinmeetOutDefaultEffort a) a b 
    a <|/\> b = meetOut (joinmeetOutDefaultEffort a) a b 

-- properties of OuterRoundedLattice (TODO)

{-|
    A type with outward-rounding lattice operations.
-}
class InnerRoundedLattice t where
    joinIn :: [EffortIndicator] -> t -> t -> t
    meetIn :: [EffortIndicator] -> t -> t -> t
    joinmeetInDefaultEffort :: t -> [EffortIndicator]

    (>|\/<) :: t -> t -> t
    (>|/\<) :: t -> t -> t
    
    a >|\/< b = joinIn (joinmeetInDefaultEffort a) a b 
    a >|/\< b = meetIn (joinmeetInDefaultEffort a) a b 


-- properties of InnerRoundedLattice (TODO)
    
-- mutable versions (TODO)    