/*
 * coeff.h
 */

#ifndef COEFF_H_
#define COEFF_H_

#define COEFF_VERSION GenericCoeff
#define ADD_COEFF_CODE(name) name##GenCf

#include "basics.h"

/* The following are provided for better code readability: */
typedef void * Coeff; // pointer to undisclosed Haskell type t

typedef void * CoeffMutable; // pointer to undisclosed Haskell type (Mutable t s)

typedef void * ComparisonOp;
   // Pointer to a Haskell value of type (Mutable t s) ->(Mutable t s) -> Int, which should be
   // a numerical comparison operator suitable for use in
   // the standard C sort function.

#define CFM_COMPARE(op, d1, d2) (eval_compare_hs(op, d1, d1))

/* References to in-place operations provided by Haskell: */
typedef struct OPS_MUTABLE
{
  Coeff sample;
  NewOpMutable new;
  CloneOpMutable clone;
  UnaryOpMutable assign;
  UnaryOpMutable assignFromPure;
  UnaryOpMutable negMutable;
  UnaryOpMutable absUpMutable;
  UnaryOpMutable absDnMutable;
  BinaryOpMutable plusUpMutable;
  BinaryOpMutable plusDnMutable;
  BinaryOpMutable minusUpMutable;
  BinaryOpMutable minusDnMutable;
  BinaryOpMutable timesUpMutable;
  BinaryOpMutable timesDnMutable;
} Ops_Mutable;

#define CFM_SAMPLE(ops) (ops -> sample)
#define CFM_NEW(ops,v) (eval_newMutable_hs(ops -> new, v))
#define CFM_CLONE(ops,rp,sp) ((rp)=eval_cloneMutable_hs(ops -> sample, ops -> clone, sp))
#define CFM_ASSIGN(ops,rp,sp) \
    (eval_assignMutable_hs(ops -> sample, ops -> assign, rp, sp))
#define CFM_ASSIGN_VAL(ops,rp,v) \
    (eval_assignMutableFromPure_hs(ops -> assignFromPure, rp, v))
#define CFM_NEG(ops,rp,dp) \
    (eval_unaryMutable_hs(ops -> sample, ops -> negMutable, rp, dp))
#define CFM_ABS_UP(ops,rp,dp) \
    (eval_unaryMutable_hs(ops -> sample, ops -> absUpMutable, rp, dp))
#define CFM_ABS_DN(ops,rp,dp) \
    (eval_unaryMutable_hs(ops -> sample, ops -> absDnMutable, rp, dp))
#define CFM_ADD_UP(ops,rp,dp1,dp2) \
    (eval_binaryMutable_hs(ops -> sample, ops -> plusUpMutable, rp, dp1, dp2))
#define CFM_ADD_DN(ops,rp,dp1,dp2) \
    (eval_binaryMutable_hs(ops -> sample, ops -> plusDnMutable, rp, dp1, dp2))
#define CFM_SUB_UP(ops,rp,dp1,dp2) \
    (eval_binaryMutable_hs(ops -> sample, ops -> minusUpMutable, rp, dp1, dp2))
#define CFM_SUB_DN(ops,rp,dp1,dp2) \
    (eval_binaryMutable_hs(ops -> sample, ops -> minusDnMutable, rp, dp1, dp2))
#define CFM_MUL_UP(ops,rp,dp1,dp2) \
    (eval_binaryMutable_hs(ops -> sample, ops -> timesUpMutable, rp, dp1, dp2))
#define CFM_MUL_DN(ops,rp,dp1,dp2) \
    (eval_binaryMutable_hs(ops -> sample, ops -> timesDnMutable, rp, dp1, dp2))

#define CFM_FREE(dp) (free_SP_hs(dp))

#endif /* COEFF_H_ */
