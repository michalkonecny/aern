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

import qualified Prelude 
import Prelude hiding (EQ, LT, GT)

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

import Numeric.AERN.Basics.RefinementOrder.Arbitrary
import Numeric.AERN.Basics.RefinementOrder.SemidecidableComparison

import Numeric.AERN.Basics.Laws.Operation
import Numeric.AERN.Basics.Laws.OperationRelation

import Data.Maybe

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

{-|
    A lattice.  Join and meet should be compatible with some partial order.
    Both operations should be idempotent, commutative and associative.
-}
class Basis t where
    (|\/?) :: t -> t -> Maybe t
    consistent :: t -> t -> Bool
    consistent a b = isJust $ a |\/? b 

(⊔?) :: (Basis t) => t -> t -> Maybe t
(⊔?) = (|\/?)

propBasisComparisonCompatible :: 
    (Eq t, SemidecidableComparison t, Basis t) => 
    t -> t -> t -> Bool
propBasisComparisonCompatible _ = 
    partialJoinOfOrderedPair (==) (|<=?) (|\/?) 

propBasisJoinAboveBoth :: 
    (SemidecidableComparison t, Basis t) => 
    t -> UniformlyOrderedPair t -> Bool
propBasisJoinAboveBoth _ (UniformlyOrderedPair (e1,e2)) = 
    partialJoinAboveOperands (|<=?) (|\/?) e1 e2

propBasisJoinIdempotent :: 
    (Eq t, Basis t) => 
    t -> t -> Bool
propBasisJoinIdempotent _ = partialIdempotent (==) (|\/?)

propBasisJoinCommutative :: 
    (Eq t, Basis t) => 
    t -> UniformlyOrderedPair t -> Bool
propBasisJoinCommutative _ (UniformlyOrderedPair (e1,e2)) = 
    partialCommutative (==) (|\/?) e1 e2

propBasisJoinAssociative :: 
    (Eq t, Basis t) => 
    t -> UniformlyOrderedTriple t -> Bool
propBasisJoinAssociative _ (UniformlyOrderedTriple (e1,e2,e3)) = 
    partialAssociative  (==) (|\/?) e1 e2 e3

testsBasis ::
    (SemidecidableComparison t,
     Basis t,
     Arbitrary t, 
     ArbitraryOrderedTuple t,
     Eq t, 
     Show t) => 
    (String, t) -> Test
testsBasis (name, sample) =
    testGroup (name ++ " (⊔?)") $
        [
         testProperty "join comparison compatible"  (propBasisComparisonCompatible sample),
         testProperty "join above both"  (propBasisJoinAboveBoth sample),
         testProperty "join idempotent" (propBasisJoinIdempotent sample),
         testProperty "join commutative" (propBasisJoinCommutative sample),
         testProperty "join associative" (propBasisJoinAssociative sample)
        ]


--{-|
--    A basis that supports in-place operations.
---}
--class (Basis t, CanBeMutable t) => BasisMutable t where
--    {-| joinMutable a b c means a := b |\/? c; a can be the same as b and/or c -}
--    maybeJoinMutable :: Mutable t s -> Mutable t s -> Mutable t s -> ST s ()
--
--    -- TODO: add default implementations using read/write
        