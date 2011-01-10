/*
 * coeff.h
 */

#ifndef COEFF_H_
#define COEFF_H_

#define COEFF_VERSION GenericCoeff

/* The following are provided for better code readability: */
typedef void * ConversionOp; // pointer to Haskell type t1 -> t2
typedef void * ComparisonOp; // pointer to Haskell type t -> t -> Int

typedef void * Coeff; // pointer to undisclosed Haskell type t
typedef void * UnaryOp; // pointer to Haskell type t -> t
typedef void * BinaryOp; // pointer to Haskell type t -> t -> t

typedef void * CoeffMutable; // pointer to undisclosed Haskell type (Mutable t s)
typedef void * NewOpMutable; // pointer to undisclosed Haskell type t -> ST s (Mutable t s)
typedef void * CloneOpMutable; // pointer to undisclosed Haskell type (Mutable t s) -> ST s (Mutable t s)
typedef void * UnaryOpMutable; // pointer to Haskell type Mutable t s -> Mutable t s -> ST s ()
typedef void * BinaryOpMutable; // pointer to Haskell type Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

/* References to operations provided by Haskell: */
typedef struct OPS_PURE
{
  UnaryOp absUp;
  UnaryOp absDn;
  BinaryOp plusUp;
  BinaryOp plusDn;
  BinaryOp minusUp;
  BinaryOp minusDn;
  BinaryOp timesUp;
  BinaryOp timesDn;
} Ops_Pure;

/* References to operations provided by Haskell: */
typedef struct OPS_MUTABLE
{
  Coeff sample;
  NewOpMutable new;
  CloneOpMutable clone;
  UnaryOpMutable assign;
  UnaryOpMutable absUpMutable;
  UnaryOpMutable absDnMutable;
  BinaryOpMutable plusUpMutable;
  BinaryOpMutable plusDnMutable;
  BinaryOpMutable minusUpMutable;
  BinaryOpMutable minusDnMutable;
  BinaryOpMutable timesUpMutable;
  BinaryOpMutable timesDnMutable;
} Ops_Mutable;


#endif /* COEFF_H_ */
