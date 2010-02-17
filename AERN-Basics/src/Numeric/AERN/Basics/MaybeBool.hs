{-|
    Module      :  Numeric.AERN.Basics.MaybeBool
    Description :  utilities for semidecidable predicates  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
-}
module Numeric.AERN.Basics.MaybeBool 

where

trueOrNothing :: Maybe Bool -> Bool 
trueOrNothing Nothing = True
trueOrNothing (Just b) = b    

notJustTrue :: Maybe Bool -> Bool 
notJustTrue Nothing = True
notJustTrue (Just b) = not b

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

 