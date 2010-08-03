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
    
{-|
   Eg: @mergeManyLists [[1,2,3],[4,5],[7]] = [1,4,7,2,5,3]@
-}    
mergeManyLists :: [[a]] -> [a]
mergeManyLists lists 
    | null listsNonempty = []
    | otherwise =
        heads ++ (mergeManyLists tails)
    where
    (heads, tails) = unzip $ map (\(h:t) -> (h,t)) listsNonempty 
    listsNonempty = filter (not . null) lists

{-|
   Like 'zip' except if the lists are non-empty and of different length,
   fill the shorter list with sufficient copies of
   its last element to make the lists of equal length.
   
   Eg @zipFill [1,2,3] [4] = [(1,4),(2,4),(3,4)]@
-}
zipFill :: [a] -> [b] -> [(a,b)] 
zipFill [] list2 = []
zipFill list1 [] = []
zipFill [h1] list2 = map (\h2 -> (h1,h2)) list2
zipFill list1 [h2] = map (\h1 -> (h1,h2)) list1
zipFill (h1:t1) (h2:t2) = (h1,h2) : (zipFill t1 t2)

{-|
   Like 'zip' except if the lists are non-empty and of different lengths,
   fill each of the shorter lists with sufficient copies of
   its last element to make all the lists of equal length.
   
   Eg: @zipFill3 [1,2,3] [4,5] [6] = [(1,4,6),(2,5,6),(3,5,6)]@
-}
zipFill3 :: [a] -> [b] -> [c] -> [(a,b,c)] 

zipFill3 [] list2 list3 = []
zipFill3 list1 [] list3 = []
zipFill3 list1 list2 [] = []

zipFill3 [h1] [h2] list3 = map (\h3 -> (h1,h2,h3)) list3
zipFill3 [h1] list2 [h3] = map (\h2 -> (h1,h2,h3)) list2
zipFill3 list1 [h2] [h3] = map (\h1 -> (h1,h2,h3)) list1

zipFill3 [h1] list2 list3 = map (\(h2,h3) -> (h1,h2,h3)) $ zipFill list2 list3
zipFill3 list1 [h2] list3 = map (\(h1,h3) -> (h1,h2,h3)) $ zipFill list1 list3
zipFill3 list1 list2 [h3] = map (\(h1,h2) -> (h1,h2,h3)) $ zipFill list1 list2

zipFill3 (h1:t1) (h2:t2) (h3:t3) = (h1,h2,h3) : (zipFill3 t1 t2 t3)

    