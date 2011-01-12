/*
 * coeff.h
 */

#ifndef COEFF_H_
#define COEFF_H_

#define COEFF_VERSION DoubleCoeff

#include <float.h>
#include <fenv.h>

/* The following are provided for better code readability: */
typedef double ConversionOp(double);

#define CF_CONVERT(convOp,d) (convOp(d))

typedef int ComparisonOp(double, double);

#define CF_COMPARE(op, d1, d2) (((d1)>(d2))? 1 : ((d1)<(d2)? -1 : 0)

typedef double Coeff;

typedef double CoeffMutable;
   // deliberately not introducing a pointer here - in C a normal
   // variable is also mutable; we will ensure
typedef double NewOpMutable(double);
typedef double CloneOpMutable(double);

/* a dummy type shadowing an important type in the generic coefficient version */
typedef struct OPS_PURE
{
} Ops_Pure;

#define ROUND_UP 2

/* the following macros give direct operation specification
 * instead of using an instance of the above type, which is
 * what has to be done for the generic coefficient version */
#define CF_ABS_UP(ops,d) (fabs(d))
#define CF_ABS_DN(ops,d) (fabs(d))
#define CF_ADD_UP(ops,d1,d2) (fesetround(ROUND_UP); ((d1)+(d2)))
#define CF_ADD_DN(ops,d1,d2) (fesetround(ROUND_UP); (-((-(d1))+(-(d2)))))
#define CF_SUB_UP(ops,d1,d2) (fesetround(ROUND_UP); ((d1)-(d2)))
#define CF_SUB_DN(ops,d1,d2) (fesetround(ROUND_UP); (-((d2)-(d1)))
#define CF_MUL_UP(ops,d1,d2) (fesetround(ROUND_UP); ((d1)*(d2)))
#define CF_MUL_DN(ops,d1,d2) (fesetround(ROUND_UP); (-((-(d1))*(d2))))

#define CF_FREE(d) (1) // signal success without doing anything
#define CF_CLONE(d) (d) // not a pointed type, normal assignment clones

// there is no need to distinguish a polynomial with mutable coefficients
// from one with immutable coefficients in the case of double implemented
// purely in C; this distinction makes sense only in Haskell

///* a dummy type shadowing an important type in the generic coefficient version */
//typedef struct OPS_MUTABLE
//{
//} Ops_Mutable;
//
///* the following macros give direct operation specification
// * instead of using an instance of the above type, which is
// * what has to be done for the generic coefficient version */
//#define CFM_SAMPLE(ops) (1.0)
//#define CFM_NEW(ops) (1.0)
//#define CFM_CLONE(ops,rp,sp) ((rp)=(sp))
//#define CFM_ASSIGN(ops,rp,sp) ((rp)=(sp))
//#define CFM_ABS_UP(ops,rp,dp) ((rp)=CF_ABS_UP(ops,dp))
//#define CFM_ABS_DN(ops,rp,dp) ((rp)=CF_ABS_DN(ops,dp))
//#define CFM_ADD_UP(ops,rp,dp1,dp2) ((rp)=CF_ADD_UP(ops,dp1,dp2))
//#define CFM_ADD_DN(ops,rp,dp1,dp2) ((rp)=CF_ADD_DN(ops,dp1,dp2))
//#define CFM_SUB_UP(ops,rp,dp1,dp2) ((rp)=CF_SUB_UP(ops,dp1,dp2))
//#define CFM_SUB_DN(ops,rp,dp1,dp2) ((rp)=CF_SUB_DN(ops,dp1,dp2))
//#define CFM_MUL_UP(ops,rp,dp1,dp2) ((rp)=CF_MUL_UP(ops,dp1,dp2))
//#define CFM_MUL_DN(ops,rp,dp1,dp2) ((rp)=CF_MUL_DN(ops,dp1,dp2))
//
//#define CFM_FREE(dp) (1)

#endif /* COEFF_H_ */
