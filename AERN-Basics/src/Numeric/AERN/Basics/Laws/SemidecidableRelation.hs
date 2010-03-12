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

import Numeric.AERN.Misc.Bool
import Numeric.AERN.Misc.Maybe
import Numeric.AERN.Basics.Laws.Operation
import Numeric.AERN.Basics.Laws.Relation
import Data.Maybe (isNothing)

--import qualified Algebra.Laws as NP -- numeric-prelude

semidecidableReflexive :: (t -> t -> Maybe Bool) -> t -> Bool
semidecidableReflexive (~~?) e = 
    trueOrNothing $ e ~~? e 
    

semidecidableTransitive :: (t -> t -> Maybe Bool) -> t -> t -> t -> Bool
semidecidableTransitive (~~?) e1 e2 e3 =
    (and $ map defined [e1 ~~? e2, e2 ~~? e3, e1 ~~? e3]) ===>
    transitive (assumeTotal2 (~~?)) e1 e2 e3

semidecidableSymmetric :: (t -> t -> Maybe Bool) -> t -> t -> Bool
semidecidableSymmetric (~~?) e1 e2 =
    (and $ map defined [e1 ~~? e2, e2 ~~? e1]) ===>
    symmetric (assumeTotal2 (~~?)) e1 e2

semidecidableOrderExtrema (<=?) bottom top e =
    (trueOrNothing $ bottom <=? e) 
    && 
    (trueOrNothing $ e <=? top)
