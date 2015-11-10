{-# LANGUAGE OverloadedLists #-}

module FnReps.Polynomial where

import Numeric.AERN.MPFRBasis.Interval

import qualified Data.HashMap.Strict as HM

{-|
    Unary polynomials over the domain [-1,1] with interval coefficients in the Chebyshev basis.
    The interval coefficients are supposed to have a very small width.
-}
data UnaryChebSparse = 
    UnaryChebSparse 
    {
        unaryChebSparse_terms :: HM.HashMap Int MI
    }
    deriving (Show)

instance Eq UnaryChebSparse where
    (==) = error "cannot compare UnaryChebSparse interval polynomials for equality, please use ==? instead of == etc."

instance Ord UnaryChebSparse where
    compare = error "cannot compare UnaryChebSparse interval polynomials for equality, please use >? instead of > etc."
    
instance Num UnaryChebSparse where
    fromInteger n = UnaryChebSparse (HM.singleton 0 (fromInteger n))
    abs _ = error $ "abs not implemented for UnaryChebSparse interval polynomials."
    signum _ = error $ "signum not implemented for UnaryChebSparse interval polynomials."
    negate (UnaryChebSparse terms) = 
        UnaryChebSparse $ fmap negate terms 
    (UnaryChebSparse termsL) + (UnaryChebSparse termsR) =
        UnaryChebSparse $ HM.unionWith (+) termsL termsR
    p1 * p2 =
        multiplyDirect_UnaryChebSparse p1 p2
        -- TODO: implement FTT-based method, see eg arxiv.org/pdf/1009.4597
        
multiplyDirect_UnaryChebSparse :: 
    UnaryChebSparse -> UnaryChebSparse -> UnaryChebSparse
multiplyDirect_UnaryChebSparse 
    (UnaryChebSparse terms1) 
    (UnaryChebSparse terms2) =
    (UnaryChebSparse terms)
    where
    terms =
        foldl addTerm HM.empty newTerms
        where
        addTerm prevTerms (i,a) = 
            HM.insertWith (+) i a prevTerms 
        newTerms =
            concat
            [   let c = a*b/2 in [(i+j, c), (abs (i-j), c)]
                | 
                (i,a) <- HM.toList terms1,
                (j,b) <- HM.toList terms2
            ]
            
        