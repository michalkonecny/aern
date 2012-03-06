{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.Basics.Arbitrary
    Description :  random generation over specific areas
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Random generation over specific areas.
-}

module Numeric.AERN.Basics.Arbitrary where

import Numeric.AERN.Basics.Consistency


import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Numeric.AERN.Misc.QuickCheck

class ArbitraryWithArea t
    where
    {-| a type of meaningful constraints to place on generation of arbitrary values -}
    type Area t
    {-| a special area that puts no constaints on the values -}
    areaWhole :: t -> Area t
    arbitraryInArea :: Area t -> Gen t
    
{-| generic mechanism for adding common restriction on the area of random generation -}
class AreaHasForbiddenValues t where
    areaAddForbiddenValues :: [t] -> Area t -> Area t
    areaGetForbiddenValues :: Area t -> [t]
class AreaHasNonNegativeOption t where
    areaRestrictToNonNeg :: t -> Area t -> Area t

class AreaHasConsistencyConstraint t where
    areaSetConsistencyConstraint :: t -> AreaConsistencyConstraint -> (Area t) -> (Area t)

data AreaConsistencyConstraint =
    AreaMaybeAllowOnlyWithConsistencyStatus (Maybe ConsistencyStatus) 

{-| an almost void, simple concrete area specification -}
data AreaWholeOnly t =
    AreaWholeOnly 
    {
        areaWholeForbiddenValues :: [t],
        areaWholeIsValueForbidden :: t -> Bool,
        areaWholeSpecialValues :: [t]
    }

areaWholeOnlyWhole specialValues =
    AreaWholeOnly [] (const False) specialValues

areaWholeOnlyAddForbiddenValues ::
    (Ord t) =>
    [t] {-^ newly forbidden values -} ->
    (AreaWholeOnly t) ->
    (AreaWholeOnly t)
areaWholeOnlyAddForbiddenValues values area =
    area
        {
            areaWholeIsValueForbidden = 
                \value ->
                    (value `elem` values)
                    ||
                    (areaWholeIsValueForbidden area value)
            ,
            areaWholeForbiddenValues =
                areaWholeForbiddenValues area ++ values
        }

arbitraryWhole ::
    (Arbitrary t) =>
    AreaWholeOnly t ->
    Gen t
arbitraryWhole (AreaWholeOnly _ isForbidden specialValues) =
    incrSize $ -- at size 0 we get only 0s...
    do
    useSpecial <- elements [False, True, False, False] 
                        -- 1 in 4 values should be special
    case useSpecial of
        True -> elements specialValues
        False -> avoidForbidden
    where 
    avoidForbidden =
        do
        result <- arbitrary
        if isForbidden result
            then avoidForbidden 
            else return result
            

    