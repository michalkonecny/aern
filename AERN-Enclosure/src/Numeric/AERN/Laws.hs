{-|
    Module      :  Numeric.AERN.Laws
    Description :  algebraic laws for properties  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Laws 
(
    idempotent, NP.commutative, NP.associative,
    reflexive, transitive, symmetric,
    semidecidableReflexive, semidecidableTransitive, semidecidableSymmetric 
)
where

import Numeric.AERN.MaybeBool
import Data.Maybe (isNothing)

import qualified Algebra.Laws as NP -- numeric-prelude

idempotent :: (Eq t) => (t -> t -> t) -> t -> Bool
idempotent op e = 
    e `op` e == e

reflexive :: (t -> t -> Bool) -> t -> Bool
reflexive rel e = 
    e `rel` e == True

symmetric :: (t -> t -> Bool) -> t -> t -> Bool
symmetric = NP.commutative

transitive :: (t -> t -> Bool) -> t -> t -> t -> Bool
transitive rel e1 e2 e3 = 
    (not (e1 `rel` e2)) || (not (e2 `rel` e3)) || (e1 `rel` e3)


semidecidableReflexive :: (t -> t -> Maybe Bool) -> t -> Bool
semidecidableReflexive rel e = 
    trueOrNothing $ e `rel` e 
    

semidecidableTransitive :: (t -> t -> Maybe Bool) -> t -> t -> t -> Bool
semidecidableTransitive rel e1 e2 e3 = 
    (notJustTrue (e1 `rel` e2)) 
    || 
    (notJustTrue (e2 `rel` e3)) 
    || 
    (trueOrNothing (e1 `rel` e3))

semidecidableSymmetric :: (t -> t -> Maybe Bool) -> t -> t -> Bool
semidecidableSymmetric rel e1 e2 =
    (isNothing $ e1 `rel` e2)
    ||
    (isNothing $ e2 `rel` e1)
    ||
    (NP.commutative rel e1 e2)

