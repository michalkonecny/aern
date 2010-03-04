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
    partialIdempotent, partialCommutative, partialAssociative, 
    modular, NP.leftDistributive,  NP.rightDistributive
)
where

import qualified Algebra.Laws as NP -- numeric-prelude

idempotent :: (Eq t) => (t -> t -> t) -> t -> Bool
idempotent (*) e = 
    e * e == e

modular (/\) (\/) e1 e2 e3 =
    (e1 /\ e3) \/ (e2 /\ e3) == ((e1 /\ e3) \/ e2) /\ e3

partialIdempotent :: (Eq t) => (t -> t -> Maybe t) -> t -> Bool
partialIdempotent (*?) e = 
    case e *? e of 
        Just r -> r == e
        _ -> True

partialCommutative :: (Eq t) => (t -> t -> Maybe t) -> t -> t -> Bool
partialCommutative (*?) e1 e2 = 
    case (e1 *? e2, e2 *? e1) of 
        (Just e12, Just e21) -> e12 == e21 
        _ -> True

partialAssociative :: (Eq t) => (t -> t -> Maybe t) -> t -> t -> t -> Bool
partialAssociative (*?) e1 e2 e3 = 
    case (e1 *? e2, e2 *? e3) of 
        (Just e12, Just e23) -> 
            case (e12 *? e3, e1 *? e23) of
                (Just eLR, Just eRL) -> eLR == eRL 
                _ -> True
        _ -> True

 