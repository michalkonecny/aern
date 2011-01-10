
module Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics where

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST, unsafeIOToST, unsafeSTToIO)
import Numeric.AERN.Basics.PartialOrdering

type UnaryOp t = t -> t
type BinaryOp t =  t -> t -> t
type ConvertOp t1 t2 = t1 -> t2
type ComparisonOp t =  t -> t -> (Maybe PartialOrdering)

type NewOpMutable s t = t -> ST s (Mutable t s)
type CloneOpMutable s t = Mutable t s -> ST s (Mutable t s)
type UnaryOpMutable s t = Mutable t s -> Mutable t s -> ST s ()
type BinaryOpMutable s t = Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

