{-|
    Module      :  Numeric.AERN.Basics.NumericOrder.Poset
    Description :  partially ordered sets using numeric order notation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Partially ordered sets using numeric order notation.
    
    This module is hidden and reexported via its parent NumericOrder. 
-}

module Numeric.AERN.Basics.NumericOrder.Poset where

import Numeric.AERN.Basics.Exception
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.NumericOrder.Extrema
import Numeric.AERN.Basics.NumericOrder.SemidecidablePoset
import Numeric.AERN.Basics.Laws.Relation

import qualified Prelude
import Prelude hiding (compare, EQ, LT, GT, (<), (<=), (>=), (>))


{-|
    A partially ordered set.
    
    (More-or-less copied from Data.Poset 
     in package altfloat-0.3 by Nick Bowler.) 
-} 
class (Eq t, SemidecidablePoset t, Show t) => Poset t where
    compare :: t -> t -> PartialOrdering
    
    -- default implementation assuming the inherited semidecidable order is actually decidable:
    compare a b =
        case maybeCompare a b of
            Just r -> r
            _ -> error $
                "poset comparison of " ++ show a
                ++ " with " ++ show b ++ " is not decidable"
    
    -- | Is comparable to.
    (<==>)  :: t -> t -> Bool
    -- | Is not comparable to.
    (</=>)  :: t -> t -> Bool
    (<)     :: t -> t -> Bool
    (<=)    :: t -> t -> Bool
    (>=)    :: t -> t -> Bool
    (>)     :: t -> t -> Bool

    -- defaults for all but compare:
    a <    b = a `compare` b == LT
    a >    b = a `compare` b == GT
    a <==> b = a `compare` b /= NC
    a </=> b = a `compare` b == NC
    a <=   b = a < b || a `compare` b == EQ
    a >=   b = a > b || a `compare` b == EQ

instance Poset Int where
    compare a b = toPartialOrdering $ Prelude.compare a b  

propPosetEqCompatible :: (Poset t) => t -> t -> Bool
propPosetEqCompatible e1 e2 =
    (e1 /= e2 || compare e1 e2 == EQ)
    &&    
    (e1 == e2 || compare e1 e2 /= EQ)

propPosetAntiSymmetric :: (Poset t) => UniformlyOrderedPair t -> Bool
propPosetAntiSymmetric (UniformlyOrderedPair (e1, e2)) = 
    compare e2 e1 == (partialOrderingTranspose $ compare e1 e2) 

propPosetTransitive :: (Poset t) => t -> t -> t -> Bool
propPosetTransitive = transitive (<=)
    
propExtremaInPoset :: (Poset t, HasExtrema t) => t -> Bool
propExtremaInPoset = extrema (<=) least highest

propPosetIllegalArgException :: (Poset t) => t -> t -> Bool
propPosetIllegalArgException illegalArg d =
    and $ map raisesAERNException 
                [compare d illegalArg, compare illegalArg d] 

