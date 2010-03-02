{-|
    Module      :  Numeric.AERN.Basics.Laws.Relation
    Description :  common properties of relations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.Laws.Relation
(
    reflexive, transitive, symmetric,
    extrema
)
where

import qualified Algebra.Laws as NP -- numeric-prelude

reflexive :: (t -> t -> Bool) -> t -> Bool
reflexive rel e = 
    e `rel` e == True

symmetric :: (t -> t -> Bool) -> t -> t -> Bool
symmetric = NP.commutative

transitive :: (t -> t -> Bool) -> t -> t -> t -> Bool
transitive rel e1 e2 e3 = 
    (not (e1 `rel` e2)) || (not (e2 `rel` e3)) || (e1 `rel` e3)

extrema (<=) bottom top e =
    bottom <= e && e <= top

