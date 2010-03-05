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

--import qualified Algebra.Laws as NP -- numeric-prelude

reflexive :: (t -> t -> Bool) -> t -> Bool
reflexive (~~) e = 
    e ~~ e == True

symmetric :: (t -> t -> Bool) -> t -> t -> Bool
symmetric (~~) e1 e2 = 
    e1 ~~ e2 == e2 ~~ e1

transitive :: (t -> t -> Bool) -> t -> t -> t -> Bool
transitive (~~) e1 e2 e3 = 
    (not (e1 ~~ e2)) || (not (e2 ~~ e3)) || (e1 ~~ e3)

extrema (<=) bottom top e =
    bottom <= e && e <= top

