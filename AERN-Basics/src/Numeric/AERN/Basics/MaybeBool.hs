{-|
    Module      :  Numeric.AERN.Basics.MaybeBool
    Description :  utilities for semidecidable predicates  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Utilities for semidecidable predicates.
-}
module Numeric.AERN.Basics.MaybeBool 

where

trueOrNothing :: Maybe Bool -> Bool 
trueOrNothing = nothingOr True

falseOrNothing :: Maybe Bool -> Bool
falseOrNothing = nothingOr False

nothingOr :: (Eq a) => a -> Maybe a -> Bool 
nothingOr _ Nothing = True
nothingOr e1 (Just e2) = e1 == e2     

justButNot :: (Eq a) => a -> Maybe a -> Bool
justButNot _ Nothing = False
justButNot e1 (Just e2) = e1 /= e2 

(||?) :: Maybe Bool -> Maybe Bool -> Maybe Bool 
(||?) b1@(Just True) _ = b1 
(||?) _ b2@(Just True) = b2
(||?) b1@(Just False) b2@(Just False) = b1
(||?) _ _ = Nothing

(&&?) :: Maybe Bool -> Maybe Bool -> Maybe Bool 
(&&?) b1@(Just False) _ = b1 
(&&?) _ b2@(Just False) = b2
(&&?) b1@(Just True) b2@(Just True) = b1
(&&?) _ _ = Nothing

 