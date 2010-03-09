{-|
    Module      :  Numeric.AERN.Basics.Laws.Relation
    Description :  common properties of semidecidable relations  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Common properties of semidecidable relations.
-}
module Numeric.AERN.Basics.Laws.SemidecidableRelation
(
    semidecidableReflexive, semidecidableTransitive, semidecidableSymmetric,
    semidecidableOrderExtrema
)
where

import Numeric.AERN.Basics.MaybeBool
import Numeric.AERN.Basics.Laws.Operation
import Data.Maybe (isNothing)

--import qualified Algebra.Laws as NP -- numeric-prelude

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
    (commutative rel e1 e2)

semidecidableOrderExtrema (<=?) bottom top e =
    (trueOrNothing $ bottom <=? e) 
    && 
    (trueOrNothing $ e <=? top)
