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
    reflexive, transitive, idempotent, NP.commutative, NP.associative
)
where

import qualified Algebra.Laws as NP -- numeric-prelude

reflexive :: (t -> t -> Bool) -> t -> Bool
reflexive rel e = 
    e `rel` e == True

transitive :: (t -> t -> Bool) -> t -> t -> t -> Bool
transitive rel e1 e2 e3 = 
    (not (e1 `rel` e2)) || (not (e2 `rel` e3)) || e1 `rel` e3

idempotent :: (Eq t) => (t -> t -> t) -> t -> Bool
idempotent op e = 
    e `op` e == e
