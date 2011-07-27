#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "GenericCoeff/coeff.h"
#include "GenericCoeff/poly.h"
#include "EvalExport_stub.h"

//#define DEBUG_FREE(x) x;
#define DEBUG_FREE(x)

void
ADD_COEFF_CODE(printPoly)(Poly *p)
{
  printf("Polynomial C-level details:\n");

  Var arity = p -> maxArity;
  Size psize = p -> psize;
  Term * terms = p -> terms;

  printf("  maxArity = %d\n", p -> maxArity);
  printf("  maxDeg = %d\n", p -> maxDeg);
  printf("  maxSize = %d\n", p -> maxSize);
  printf("  maxTermArity = %d\n", p -> maxTermArity);
  printf("    constant term addr = %p\n", p -> constTerm);
  printf("  psize = %d\n", psize);

  for (int i = 0; i < psize; i++)
    {
      printf("    term ");
      printf(" monomial degree = %d,", MONOMIAL_DEGREE(terms[i].powers));
      printf(" arity = %d\n", TERM_ARITY(terms[i].powers));
      FOREACH_VAR_ARITY(var, arity)
        {
          printf("[%d]", POWER_OF_VAR(terms[i].powers, var));
        }
      printf(" coeff addr = %p\n", terms[i].coeff);
    }
}

#define CHECK_NOT(cond,msg) \
    if(cond) \
      { \
        printf msg; \
        return 1; \
      }

int
ADD_COEFF_CODE(checkPoly)(Ops * ops, Poly * p)
{
  Var arity = p -> maxArity;
  CHECK_NOT(arity < 0, ("checkPoly: negative arity %d\n", arity));
  CHECK_NOT(arity > 1000, ("checkPoly: too large arity %d\n", arity));

  Power maxDeg = p -> maxDeg;
  CHECK_NOT(maxDeg < 0, ("checkPoly: negative maxDeg %d\n", maxDeg));
  CHECK_NOT(maxDeg > 1000, ("checkPoly: too large maxDeg %d\n", maxDeg));

  Size maxSize = p -> maxSize;
  CHECK_NOT(maxSize < 0, ("checkPoly: negative maxSize %d\n", maxSize));
  CHECK_NOT(maxSize > 10000, ("checkPoly: too large maxSize %d\n", maxSize));

  Var maxTermArity = p -> maxTermArity;
  CHECK_NOT(maxTermArity < 0, ("checkPoly: negative maxTermArity %d\n", maxTermArity));
  CHECK_NOT(maxTermArity > 14, ("checkPoly: too large maxTermArity %d\n", maxTermArity));
//  CHECK_NOT(maxTermArity > arity, ("checkPoly: maxTermArity %d > arity %d\n", maxTermArity, arity));

  Size psize = p -> psize;
  CHECK_NOT(psize < 0, ("checkPoly: negative psize %d\n", psize));
  CHECK_NOT(psize > maxSize, ("checkPoly: too large psize %d\n", psize));

  CHECK_NOT(p -> terms == NULL, ("checkPoly: terms are NULL\n"));
  Term * terms = p -> terms;

  CFM_IS_LEGAL(ops, p -> constTerm);

  for (int i = 0; i < psize; i++)
    {
      // compute term's power and term arity:
      int dg = 0;
      int ta = 0;
      Power * powers = terms[i].powers;
      FOREACH_VAR_ARITY(var,arity)
        {
          int pw = POWER_OF_VAR(powers, var);
          dg += pw;
          if(pw){ ta++; }
        }

      // check powers add up to monomial degree:
      CHECK_NOT(dg != MONOMIAL_DEGREE(powers),
         ("checkPoly: degree in powers for term %d is %d should be %d", i, MONOMIAL_DEGREE(powers), dg));

      // check internal arity is correct:
      CHECK_NOT(ta != TERM_ARITY(powers),
          ("checkPoly: term arity in powers for term %d is %d should be %d", i, TERM_ARITY(powers), ta));

      // check term degree <= maxDeg:
      CHECK_NOT(dg > maxDeg,
         ("checkPoly: term %d degree %d higher than the limit %d", i, dg, maxDeg));

      // check term arity <= maxTermArity:
      CHECK_NOT(ta > maxTermArity,
         ("checkPoly: term %d arity %d higher than the limit %d", i, ta, maxTermArity));

      // check coeff:
      CFM_IS_LEGAL(ops, terms[i].coeff);
    }

  // check there is no aliasing among powers:
  // TODO

  // check there is no aliasing among coeffs:
  // TODO

  return 0;
}

void
ADD_COEFF_CODE(freePoly)(Poly *p)
{
  DEBUG_FREE(printf("freePoly: starting\n"));

  // free the Poly struct:
  Size maxSize = p -> maxSize;
  Size psize = p -> psize;
  Term * terms = p -> terms;
  CFM_FREE(p -> constTerm);
  CFM_FREE(p -> errorBound);
  free(p);

  Size i = 0;
  // free the power arrays:
  while (i < psize)
    {
      free(terms[i].powers);
      CFM_FREE(terms[i].coeff);
      i++;
    }

  // free the coeffs:
  while (i < maxSize)
    {
      free(terms[i].powers);
      i++;
    }

  // free the terms array:
  free(terms);

  DEBUG_FREE(printf("freePoly: finishing\n"));

}

void
ADD_COEFF_CODE(mapCoeffsInPlace)(ConversionOp convert, Poly *p)
{
  Coeff temp = p -> constTerm;
  p -> constTerm = CFM_CONVERT(convert, p -> constTerm);
  CFM_FREE(temp);

  temp = p -> errorBound;
  p -> errorBound = CFM_CONVERT(convert, p -> errorBound);
  CFM_FREE(temp);

  Size psize = p -> psize;
  Term * terms = p -> terms;
  for (Size i = 0; i < psize; ++i)
    {
      temp = terms[i].coeff;
      terms[i].coeff = CFM_CONVERT(convert, terms[i].coeff);
      CFM_FREE(temp);
    }
}

Poly *
ADD_COEFF_CODE(newConstPoly)(Coeff c, Coeff errorBound, Var maxArity,
    Size maxSize, Power maxDeg, Var maxTermArity)
{
  //  printf("newConstPoly: starting\n");
  Poly * poly = (Poly *) malloc(sizeof(Poly));
  poly -> maxArity = maxArity;
  poly -> maxSize = maxSize;
  poly -> maxDeg = maxDeg;
  poly -> maxTermArity = maxTermArity;
  poly -> errorBound = errorBound;
  poly -> psize = 0;
  poly -> constTerm = c;

  if (maxSize > 0)
    {
      poly -> terms = malloc(maxSize * sizeof(Term));

      // allocate space for terms' powers:
      for (Size i = 0; i < maxSize; i++)
        {
          (poly -> terms)[i].powers = (Power *) malloc(SIZEOF_POWERS(maxArity));
          // no need to initialise powers and
          // coefficients as these terms are inactive
        }
    }
  else
    {
      poly -> terms = malloc(1); // dummy to avoid having to check for NULL in many places
    }

  //  printf("newConstPoly: returning\n");
  return poly;
}

Poly *
ADD_COEFF_CODE(newTestPoly)(Coeff c, Coeff a0, Coeff a1, Coeff errorBound)
{
  //  printf("newConstPoly: starting\n");
  Poly * poly = (Poly *) malloc(sizeof(Poly));
  poly -> maxArity = 2;
  poly -> maxSize = 2;
  poly -> maxDeg = 3;
  poly -> maxTermArity = 3;
  poly -> errorBound = errorBound;
  poly -> constTerm = c;
  poly -> psize = 2;
  poly -> terms = malloc(poly -> maxSize * sizeof(Term));

  Power * powers0 = (Power *) malloc(SIZEOF_POWERS(poly -> maxArity));
  MONOMIAL_DEGREE(powers0) = 1;
  TERM_ARITY(powers0) = 1;
  POWER_OF_VAR(powers0, 0) = 1;
  POWER_OF_VAR(powers0, 1) = 0;
  (poly -> terms)[0].powers = powers0;
  (poly -> terms)[0].coeff = a0;

  Power * powers1 = (Power *) malloc(SIZEOF_POWERS(poly -> maxArity));
  MONOMIAL_DEGREE(powers1) = 3;
  TERM_ARITY(powers1) = 2;
  POWER_OF_VAR(powers1, 0) = 2;
  POWER_OF_VAR(powers1, 1) = 1;
  (poly -> terms)[1].powers = powers1;
  (poly -> terms)[1].coeff = a1;

  // no need to initialise powers and
  // coefficients as these terms are inactive

  //  printf("newConstPoly: returning\n");
  return poly;
}

// ASSUMES: 0 <= var < maxArity
// ASSUMES: 0 < maxSize
// ASSUMES: 0 < maxDeg
// ASSUMES: 0 < maxTermArity <= 14

Poly *
ADD_COEFF_CODE(newProjectionPoly)(Coeff zero, Coeff one, Coeff errorBound,
    Var var, Var maxArity, Size maxSize, Power maxDeg, Var maxTermArity)
{
  Poly * poly = ADD_COEFF_CODE(newConstPoly)(zero, errorBound, maxArity,
      maxSize, maxDeg, maxTermArity);

  // add one term for the variable:
  poly -> psize = 1;
  Term * term = poly -> terms;

  // initialise the term's coeff:
  term -> coeff = one;
  //  printf("newProjectionPoly: coeff one address = %p\n", term -> coeff);

  // initialise the term's powers:
  Power * powers = term -> powers;

  // initialise the "cache" of the monomial degree:
  MONOMIAL_DEGREE(powers) = 1;

  // initialise the "cache" of the term arity:
  TERM_ARITY(powers) = 1;

  // all zero:
  FOREACH_VAR_ARITY(v,maxArity)
    {
      POWER_OF_VAR(powers,v) = 0;
    }

  // except the chosen var:
  POWER_OF_VAR(powers,var) = 1;

  return poly;
}
