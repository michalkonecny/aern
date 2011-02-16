/*
 * coeff.h
 */

#ifndef COEFF_H_
#define COEFF_H_

#define COEFF_VERSION DoubleCoeff
#define ADD_COEFF_CODE(name) name##DblCf

#include <math.h>
#include <float.h>
#include <fenv.h>

#include "basics.h"

/* The following are provided for better code readability: */
typedef double Coeff;
typedef double CoeffMutable;

/* dummy type to keep similar function signatures as in the GenericCoeff version */
typedef void * ComparisonOp;

#define CF_COMPARE(op, d1, d2) (((d1)>(d2))? 1 : ((d1)<(d2)? -1 : 0))

/* a dummy type shadowing an important type in the generic coefficient version */
typedef struct OPS_PURE
{
} Ops_Pure;

typedef Ops_Pure Ops_Mutable;

#define ROUND_UP 2

/* the following macros give direct operation specification
 * instead of using an instance of the above type, which is
 * what has to be done for the generic coefficient version */
#define CF_ABS_UP(ops,d) (fabs(d))
#define CF_ABS_DN(ops,d) (fabs(d))
#define CF_ADD_UP(ops,d1,d2) (fesetround(ROUND_UP), ((d1)+(d2)))
#define CF_ADD_DN(ops,d1,d2) (fesetround(ROUND_UP), (-((-(d1))+(-(d2)))))
#define CF_SUB_UP(ops,d1,d2) (fesetround(ROUND_UP), ((d1)-(d2)))
#define CF_SUB_DN(ops,d1,d2) (fesetround(ROUND_UP), (-((d2)-(d1))))
#define CF_MUL_UP(ops,d1,d2) (fesetround(ROUND_UP), ((d1)*(d2)))
#define CF_MUL_DN(ops,d1,d2) (fesetround(ROUND_UP), (-((-(d1))*(d2))))
#define CF_NEG(ops,d) (-d)

#define CF_FREE(d) (1) // signal success without doing anything
#define CF_CLONE(d) (d) // not a pointer type, normal assignment clones

// there is no need to distinguish a polynomial with mutable coefficients
// from one with immutable coefficients in the case of double implemented
// purely in C; this distinction makes sense only in Haskell

// nevertheless, the mutable algorithms are likely to be a little more
// efficient than the pure ones

#define CFM_SAMPLE(ops) (0.0)
#define CFM_NEW(ops,v) (v)
#define CFM_CLONE(ops,rp,sp) ((rp)=(sp))
#define CFM_ASSIGN(ops,rp,sp) \
    ((rp)=(sp))
#define CFM_ASSIGN_VAL(ops,rp,v) \
    ((rp)=(v))
#define CFM_ABS_UP(ops,rp,dp) \
    ((rp)=CF_ABS_UP(ops,dp))
#define CFM_ABS_DN(ops,rp,dp) \
    ((rp)=CF_ABS_DN(ops,dp))
#define CFM_ADD_UP(ops,rp,dp1,dp2) \
    ((rp)=CF_ADD_UP(ops,dp1,dp2))
#define CFM_ADD_DN(ops,rp,dp1,dp2) \
    ((rp)=CF_ADD_DN(ops,dp1,dp2))
#define CFM_SUB_UP(ops,rp,dp1,dp2) \
    ((rp)=CF_SUB_UP(ops,dp1,dp2))
#define CFM_SUB_DN(ops,rp,dp1,dp2) \
    ((rp)=CF_SUB_DN(ops,dp1,dp2))
#define CFM_MUL_UP(ops,rp,dp1,dp2) \
    ((rp)=CF_MUL_UP(ops,dp1,dp2))
#define CFM_MUL_DN(ops,rp,dp1,dp2) \
    ((rp)=CF_MUL_DN(ops,dp1,dp2))

#define CFM_FREE(dp) (0)


#endif /* COEFF_H_ */
