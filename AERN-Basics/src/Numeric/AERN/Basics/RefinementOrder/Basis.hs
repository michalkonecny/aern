{-|
    Module      :  Numeric.AERN.Basics.RefinementOrder.Basis
    Description :  domain bases using refinement order notation  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Domain bases using refinement order notation.
    
    This module is hidden and reexported via its parent RefinementOrder. 
-}

module Numeric.AERN.Basics.RefinementOrder.Basis where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.RefinementOrder.Poset
import Numeric.AERN.Basics.Laws.Operation
import Numeric.AERN.Basics.Laws.OperationRelation

import Data.Maybe

import qualified Prelude 
import Prelude hiding (EQ, LT, GT)

{-|
    A lattice.  Join and meet should be compatible with some partial order.
    Both operations should be idempotent, commutative and associative.
-}
class (Eq t) => Basis t where
    (|\/?) :: t -> t -> Maybe t
    consistent :: t -> t -> Bool
    consistent a b = isJust $ a |\/? b 

(⊔?) :: (Basis t) => t -> t -> Maybe t
(⊔?) = (|\/?)

propBasisPosetCompatible :: (Poset t, Basis t) => t -> t -> Bool
propBasisPosetCompatible = partialJoinOfOrderedPair (==) (|<=) (|\/?) 

propBasisJoinAboveBoth :: (Poset t, Basis t) => t -> t -> Bool
propBasisJoinAboveBoth = partialJoinAboveOperands (|<=) (|\/?)

propBasisJoinIdempotent :: (Basis t) => t -> Bool
propBasisJoinIdempotent = partialIdempotent (==) (|\/?)

propBasisJoinCommutative :: (Basis t) => t -> t -> Bool
propBasisJoinCommutative = partialCommutative (==) (|\/?)

propBasisJoinAssocative :: (Basis t) => t -> t -> t -> Bool
propBasisJoinAssocative = partialAssociative  (==) (|\/?)


{-|
    A basis that supports in-place operations.
-}
class (Basis t, CanBeMutable t) => BasisMutable t where
    {-| joinMutable a b c means a := b |\/? c; a can be the same as b and/or c -}
    maybeJoinMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

    -- TODO: add default implementations using read/write
        