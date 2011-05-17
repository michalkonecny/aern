/*
 * haskell_fn_types.h
 */

#ifndef HASKELL_FN_TYPES_H_
#define HASKELL_FN_TYPES_H_

#include <stdint.h>

typedef uint32_t Var;
/* variables in polynomials range from 0 to arity-1 */

#define FOREACH_VAR_ARITY(var,arity) for(Var var = 0; var < arity; var++)

typedef uint8_t Power;
typedef uint32_t Size;

/* The following are provided for better code readability: */

typedef void * ConversionOp; // pointer to Haskell type t1 -> t2

#define CFM_CONVERT(convOp,d) (eval_convert_hs(convOp, d))


typedef void * UnaryOp; // pointer to Haskell type t -> t
typedef void * BinaryOp; // pointer to Haskell type t -> t -> t

typedef void * NewOpMutable; // pointer to undisclosed Haskell type t -> ST s (Mutable t s)
typedef void * CloneOpMutable; // pointer to undisclosed Haskell type (Mutable t s) -> ST s (Mutable t s)
typedef void * UnaryOpMutable; // pointer to Haskell type Mutable t s -> Mutable t s -> ST s ()
typedef void * BinaryOpMutable; // pointer to Haskell type Mutable t s -> Mutable t s -> Mutable t s -> ST s ()

#endif /* HASKELL_FN_TYPES_H_ */
