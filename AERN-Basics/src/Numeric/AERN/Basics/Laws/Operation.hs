{-|
    Module      :  Numeric.AERN.Basics.Laws.Relation
    Description :  common properties of binary operations 
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.Laws.Operation
(
    idempotent, NP.commutative, NP.associative, 
    modular, NP.leftDistributive,  NP.rightDistributive
)
where

import qualified Algebra.Laws as NP -- numeric-prelude

idempotent :: (Eq t) => (t -> t -> t) -> t -> Bool
idempotent (*) e = 
    e * e == e

modular (/\) (\/) e1 e2 e3 =
    (e1 /\ e3) \/ (e2 /\ e3) == ((e1 /\ e3) \/ e2) /\ e3
