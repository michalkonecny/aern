{-|
    Module      :  Numeric.AERN.Misc.List
    Description :  miscellaneous list functions  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Miscellaneous list functions.
-}
module Numeric.AERN.Misc.List where

import qualified Data.List as List

sortUsing :: (Ord b) => (a -> b) -> [a] -> [a]
sortUsing f =
    List.sortBy compareF
    where
    compareF a b = compare (f a) (f b)

{-|
    Eg: @combinations [[1,2,3],[4,5],[7]] = [[1,4,7], [1,5,7], [2,4,7], [2,5,7], [3,4,7], [3,5,7]]@
-}
combinations :: [[a]] -> [[a]]
combinations [] = [[]]
combinations (options : rest) =
    concat $ map addHeadToAll options 
    where
    addHeadToAll h = map (h :) restDone
    restDone = combinations rest 
    
    
mergeManyLists :: [[a]] -> [a]
mergeManyLists lists 
    | null listsNonempty = []
    | otherwise =
        heads ++ (mergeManyLists tails)
    where
    (heads, tails) = unzip $ map (\(h:t) -> (h,t)) listsNonempty 
    listsNonempty = filter (not . null) lists
