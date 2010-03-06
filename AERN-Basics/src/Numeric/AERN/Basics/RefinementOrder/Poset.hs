{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.Poset
    Description :  partially ordered sets using refinement order notation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}

module Numeric.AERN.Basics.RefinementOrder.Poset where

import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.RefinementOrder.Extrema
import Numeric.AERN.Basics.Laws.Relation

import qualified Prelude 
import Prelude hiding (compare, EQ, LT, GT)
import Test.QuickCheck


{-|
    A partially ordered set.
    
    (More-or-less copied from Data.Poset 
     in package altfloat-0.3 by Nick Bowler.) 
-} 
class (Eq t) => Poset t where
    compare :: t -> t -> PartialOrdering
    -- | Is comparable to.
    (|<==>)  :: t -> t -> Bool
    -- | Is not comparable to.
    (|</=>)  :: t -> t -> Bool
    (|<)     :: t -> t -> Bool
    (|<=)    :: t -> t -> Bool
    (|>=)    :: t -> t -> Bool
    (|>)     :: t -> t -> Bool

    -- defaults for all but compare:
    a |<    b = a `compare` b == LT
    a |>    b = a `compare` b == GT
    a |<==> b = a `compare` b /= NC
    a |</=> b = a `compare` b == NC
    a |<=   b = a |< b || a `compare` b == EQ
    a |>=   b = a |> b || a `compare` b == EQ

-- convenience Unicode math operator notation:
(⊏) :: (Poset t) => t -> t -> Bool
(⊏) = (|<)
(⊑) :: (Poset t) => t -> t -> Bool
(⊑) = (|<=)
(⊒) :: (Poset t) => t -> t -> Bool
(⊒) = (|>=)
(⊐) :: (Poset t) => t -> t -> Bool
(⊐) = (|>)

propPosetEqCompatible :: (Poset t) => t -> t -> Bool
propPosetEqCompatible e1 e2 =
    (e1 /= e2 || compare e1 e2 == EQ)
    &&    
    (e1 == e2 || compare e1 e2 /= EQ)

propPosetAntiSymmetric :: (Poset t) => t -> t -> Bool
propPosetAntiSymmetric e1 e2 = 
    compare e2 e1 == (partialOrderingTranspose $ compare e1 e2) 

propPosetTransitive :: (Poset t) => t -> t -> t -> Bool
propPosetTransitive = transitive (⊑)
    
propExtremaInPoset :: (Poset t, HasExtrema t) => t -> Bool
propExtremaInPoset = extrema (⊑) (⊥) (⊤)
    