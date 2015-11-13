{-# LANGUAGE OverloadedLists #-}

module FnReps.Polynomial.UnaryChebSparse where

import FnReps.Polynomial.UnaryChebSparse.DCTMultiplication

--import Numeric.AERN.MPFRBasis.Interval
import Numeric.AERN.DoubleBasis.Interval

import qualified Data.HashMap.Strict as HM

--type RA = DI

{-|
    Unary polynomials over the domain [-1,1] with interval coefficients in the Chebyshev basis.
    The interval coefficients are supposed to have a very small width.
-}
data UnaryChebSparse = 
    UnaryChebSparse 
    {
        unaryChebSparse_terms :: HM.HashMap Int RA
    }
--    deriving (Show)

instance Show UnaryChebSparse where
    show (UnaryChebSparse terms) =
        "(UnaryChebSparse " ++ show (HM.toList terms) ++ ")"  

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
        multiplyDirect p1 p2
        -- TODO: implement FTT-based method, see eg arxiv.org/pdf/1009.4597
        
multiplyDirect :: 
    UnaryChebSparse -> UnaryChebSparse -> UnaryChebSparse
multiplyDirect 
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
            
multiplyDCT ::
    UnaryChebSparse -> UnaryChebSparse -> UnaryChebSparse
multiplyDCT 
    (UnaryChebSparse terms1) 
    (UnaryChebSparse terms2) =
    (UnaryChebSparse terms)
    where
    terms = multiplyDCT_terms terms1 terms2

