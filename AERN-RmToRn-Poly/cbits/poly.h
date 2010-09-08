#include <stdint.h>

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

typedef uint32_t Var;
typedef uint32_t Power;
typedef uint32_t Size;

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

/*
 * polynomial term
 *
 * invariants:
 *   the power array is private to one instance of Term
 *
 *   the coeff is private to one instance of Term
 */
typedef struct TERM
{
  Power * powers;
  Coeff coeff;
} Term;

/*
 * polynomial
 *
 * invariants:
 *   all terms are private to one instance of Poly,
 *   no aliasing allowed internally nor externally
 *
 *   the constTerm is private to one instance of Poly
 */
typedef struct POLY
{
  Var maxArity; // nominal number of variables
  Size maxSize; // maximal number of non-constant terms
  Size psize; // actual number of non-constant terms
  Coeff constTerm;
  Term * terms;
} Poly;

void
freePoly(Poly *p);

/*
 * preconditions:
 * no persistent references to c and this pointer is unique for each call
 */
Poly *
newConstPoly(const Coeff c, Var maxArity, Size maxSize);

/*
 * preconditions:
 * 0 <= var && var < maxArity
 * 0 < maxSize
 * no persistent references to zero, one and these pointers are unique to each call
 */
Poly *
newProjectionPoly(const Coeff zero, const Coeff one, Var var, Var maxArity,
    Size maxSize);

///*
// * preconditions:
// * res has to point to an allocated polynomial with initialised maxArity and maxSize;
// * res has to have maxArity larger than p;
// * res has to have maxSize not smaller than p;
// */
//void
//incrementArity(Poly * res, Poly *p, Var * old2new);

/*
 * The following operations expect all polynomial parameters and result space to
 * have matching maxArities.
 *
 * All Coeff parameters passed with a call are deallocated during the call.
 */

void
addUp(Coeff zero, const ComparisonOp compare, const Ops_Pure * ops,
    Poly *res, const Poly * p1, const Poly * p2);

void
addDn(Coeff zero, const ComparisonOp compare, const Ops_Pure * ops,
    Poly *res, const Poly * p1, const Poly * p2);

void
addUpMutable(Coeff zero, const ComparisonOp compare, const Ops_Mutable * ops,
    Poly *res, const Poly * p1, const Poly * p2);

void
testAssign(Coeff sample, UnaryOpMutable assign, CoeffMutable to, CoeffMutable from);

typedef void * Value; // A Haskell value passed via StablePtr

/*
 * Interpret the terms as Chebyshev polynomials and evaluate them for the
 * given values assigned to variables using the given addition and multiplication
 * Haskell operations.
 */
Value
evalAtPtChebBasis(const Poly *, const Value *, const Value, const BinaryOp,
    const BinaryOp, const BinaryOp, const ConversionOp);

