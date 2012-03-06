{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Laws
    Description :  auxiliary functions for testable properties  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Auxiliary functions for testable properties.
-}

module Numeric.AERN.RmToRn.Laws where

import Numeric.AERN.RmToRn.Domain

import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Misc.Debug

roundedRefinementIsotoneDom ::
    (RefOrd.PartialComparison t, 
     RefOrd.ArbitraryOrderedTuple (DomainBox f),
     Show (DomainBox f),
     Show (Domain f),
     Show t)
    =>
    f ->
    String ->
    (ei -> (DomainBox f) -> t) ->
    (ei -> (DomainBox f) -> t) ->
    (RefOrd.LEPair (DomainBox f)) -> 
    ei -> 
    (RefOrd.PartialCompareEffortIndicator t) ->
    Bool
roundedRefinementIsotoneDom _ contextDescription exprUp exprDn (RefOrd.LEPair (domDn, domUp)) effort effortComp =
--    unsafePrint
--    (
--        "roundedRefinementIsotoneDom: "
--        ++ "\n domUp = " ++ show domUp
--        ++ "\n domDn = " ++ show domDn
--        ++ "\n resUp = " ++ show resUp
--        ++ "\n resDn = " ++ show resDn
--    ) $
    case RefOrd.pLeqEff effortComp resDn resUp of
        Just True -> True
        Just False -> 
            unsafePrint
            (
                "roundedRefinementIsotoneDom failed for:"
                ++ "\n domUp = " ++ show domUp
                ++ "\n domDn = " ++ show domDn
                ++ "\n resUp = " ++ show resUp
                ++ "\n resDn = " ++ show resDn
            )
            False
        _ -> True
    where
    resUp = check $ exprUp effort domUp
    resDn = check $ exprDn effort domDn
    check = id -- detectIllegalValues $ contextDescription ++ " refinement isotone"
    
