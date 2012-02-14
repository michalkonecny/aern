{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Domain
    Description :  operations focusing on function domains  
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    Operations focusing on function domains.
-}

module Numeric.AERN.RmToRn.Domain where

--import Numeric.AERN.Basics.Interval
import qualified Numeric.AERN.RealArithmetic.RefinementOrderRounding as ArithInOut
import qualified Numeric.AERN.RefinementOrder as RefOrd

import Numeric.AERN.Misc.Debug

--import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

type DomainBox f = VarBox f (Domain f)

class 
    (HasVarValue 
        (DomainBox f) 
        (Var f) 
        (Domain f))
    => 
    HasDomainBox f 
    where
    type Var f
    type Domain f
    type VarBox f :: * -> *
--    getSampleVar :: f -> Var f
    getSampleDomValue :: f -> Domain f
    defaultDomSplit ::
        f {-^ dummy parameter that aids typechecking -} -> 
        (Domain f) {-^ domain area to split -} -> 
        (Domain f, Domain f) 
            {-^ A partition of size 2 of the given area. 
                The two parts may overlap. -}
    getDomainBox :: 
        f {-^ a function @f@ -} -> 
        DomainBox f 
            {-^ the domain of definition of @f@, 
                usually explicitly encoded inside @f@ -}
    getNSamplesFromDomainBox ::
        f {-^ dummy parameter that aids typechecking -} -> 
        (DomainBox f) {-^ @dom@ -} -> 
        Int {-^ @n@ -} -> 
        [DomainBox f] 
            {-^ a list of @n@ points (ie thin boxes) in @dom@, 
                relatively well interspersed over @dom@, 
                typically including some points on the boundary -}
    getSampleFromInsideDomainBox ::
        f {-^ dummy parameter that aids typechecking -} -> 
        (DomainBox f) {-^ @dom@ -} -> 
        DomainBox f 
            {-^ a point (ie thin boxe) inside @dom@, 
                ie not on the boundary -}


defaultDomSplitUsingEndpointsDefaultEffort dom =
    defaultDomSplitUsingEndpointsEff (effFromE, effGetE, effAdd, effIntDiv) dom
    where
    effFromE = RefOrd.fromEndpointsDefaultEffort dom
    effGetE = RefOrd.getEndpointsDefaultEffort dom
    effAdd = ArithInOut.addDefaultEffort dom
    effIntDiv = ArithInOut.mixedDivDefaultEffort dom (1 :: Int)
    
defaultDomSplitUsingEndpointsEff (effFromE, effGetE, effAdd, effIntDiv) dom =
    (domL, domR)
    where
    domL = RefOrd.fromEndpointsOutEff effFromE (domLE, domME)
    domR = RefOrd.fromEndpointsOutEff effFromE (domME, domRE)
    domME = 
        let (<+>) = ArithInOut.addOutEff effAdd in 
        let (</>|) = ArithInOut.mixedDivOutEff effIntDiv in 
        (domLE <+> domRE) </>| (2::Int)
    (domLE, domRE) = RefOrd.getEndpointsOutEff effGetE dom

getNSamplesFromDomainBoxUsingEndpointsDefaultEffort dom =
    getNSamplesFromDomainBoxUsingEndpointsEff effGetE
    where
    effGetE = RefOrd.getEndpointsDefaultEffort dom
--    effFromE = RefOrd.fromEndpointsDefaultEffort dom
--    effAdd = ArithInOut.addDefaultEffort dom
--    effIntDiv = ArithInOut.mixedDivDefaultEffort dom (1 :: Int)
    
getNSamplesFromDomainBoxUsingEndpointsEff 
        effGetE
        sampleF
        dombox n =
    take n $ samples
    where
    samples = 
        gsbAux (emptyVarBox dombox) dombox
        where
        gsbAux setVars dombox =
            case splitOffVar dombox of
                Nothing -> [setVars] -- empty dombox
                Just ((var,dom), domboxR) ->
                    mergeListsOfLists $ map auxRec domPoints
                    where
                    auxRec domPoint =
                        gsbAux (insertVar var domPoint setVars) domboxR
                    domPoints = getSamplesFromDom dom
    getSamplesFromDom dom =
        gsdAux (True, True) dom
        where
        gsdAux (takeLEndpoint, takeREndpoint) dom =
--            unsafePrint ("gsdAux: dom = " ++ show dom) $ 
            result
            where
            result
                | takeLEndpoint && takeREndpoint = domLE : domRE : rec
                | takeLEndpoint = domLE : rec
                | takeREndpoint = domRE : rec
                | otherwise = rec
            (domLE, domRE) = RefOrd.getEndpointsOutEff effGetE dom
            rec = mergeTwoLists recL recR
            recL = gsdAux (False, True) domL
            recR = gsdAux (False, False) domR
            (domL, domR) = defaultDomSplit sampleF dom

mergeTwoLists :: [a] -> [a] -> [a]
mergeTwoLists [] l = l
mergeTwoLists (h1:t1) l = 
    h1 : (aux l)
    where
    aux [] = t1
    aux (h2:t2) = h2:(mergeTwoLists t1 t2)

mergeListsOfLists :: [[a]] -> [a]
mergeListsOfLists lists = aux 1 lists
    where
    aux n [] = []
    aux n lists =
        nElems ++ (aux (n+1) listsUpdated)
        where
        listsUpdated = nListsUpdated ++ remainingLists
        (nLists, remainingLists) = splitAt n lists
        (nElems, nListsUpdated) = scanHeads nLists
        scanHeads [] = ([], [])
        scanHeads (headList : remLists) =
            case headList of
                [] -> (elems, lists)
                (h : t) -> (h : elems, t : lists)
            where 
            (elems, lists) = scanHeads remLists

getSampleFromInsideDomainBoxUsingEndpointsDefaultEffort dom =
    getSampleFromInsideDomainBoxUsingEndpointsEff effGetE
    where
    effGetE = RefOrd.getEndpointsDefaultEffort dom

getSampleFromInsideDomainBoxUsingEndpointsEff 
        effGetE
        sampleF
        dombox
    =
    centre
    where
    centre = 
        fromAscList $ map computeDomCentre $ toAscList dombox
        where
        computeDomCentre (var, dom) =
            (var, domME)
            where
            (domME, _) = RefOrd.getEndpointsOutEff effGetE domR
            (domL, domR) = defaultDomSplit sampleF dom

class HasVarValue vbox var val 
    | vbox -> var val
    where
    emptyVarBox :: vbox -> vbox
    unaryVarBox :: var -> val -> vbox
    mapVarBox :: (val -> val) -> vbox -> vbox 
    fromList :: [(var, val)] -> vbox
    fromAscList :: [(var, val)] -> vbox
    toAscList :: vbox -> [(var, val)]
    getVars :: vbox -> [var]
    lookupVar :: vbox -> var -> Maybe val
    splitOffVar :: vbox -> Maybe ((var, val), vbox)
    insertVar :: var -> val -> vbox -> vbox
    -- TODO add much more (see hsreals DomainBox)


instance HasVarValue (IntMap.IntMap val) Int val
    where
    emptyVarBox _ = IntMap.empty
    unaryVarBox = IntMap.singleton
    mapVarBox = IntMap.map
    fromList varVals = IntMap.fromList varVals
    fromAscList varVals = IntMap.fromAscList varVals
    toAscList vbox = IntMap.toAscList vbox
    getVars vbox = IntMap.keys vbox
    lookupVar map var = IntMap.lookup var map
    splitOffVar = IntMap.minViewWithKey
    insertVar = IntMap.insert
        

instance (Ord var) => HasVarValue (Map.Map var val) var val
    where
    emptyVarBox _ = Map.empty
    unaryVarBox = Map.singleton
    mapVarBox = Map.map
    fromList varVals = Map.fromList varVals
    fromAscList varVals = Map.fromAscList varVals
    toAscList vbox = Map.toAscList vbox
    getVars vbox = Map.keys vbox
    lookupVar map var = Map.lookup var map 
    splitOffVar = Map.minViewWithKey
    insertVar = Map.insert
     
           
class GeneratableVariables var 
    where
--    getFreshVariable :: Set.Set var -> var
    getNVariables :: Int -> [var]
    
instance GeneratableVariables String
    where
    getNVariables n 
        | n <= 3 = take n ["x","y","z"]
        | otherwise =
            take n $ map (\i -> "x" ++ show i) [1..]  

instance GeneratableVariables Int
    where
    getNVariables n = [1..n] 
        