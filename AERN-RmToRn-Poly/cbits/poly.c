#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "poly.h"
#include "EvalExport_stub.h"

void
freePoly(Poly *p)
{
  // free the Poly struct:
  Size maxSize = p -> maxSize;
  Term * terms = p -> terms;
  free_SP_hs(p -> constTerm);
  free(p);

  // free the power arrays and coeffs:
  for (Size i = 0; i < maxSize; i++)
    {
      free(terms[i].powers);
      free_SP_hs(terms[i].coeff);
    }

  // free the terms array:
  free(terms);
}

void
mapCoeffsInPlace(ConversionOp convert, Poly *p)
{
  p -> constTerm = eval_convert_hs(convert, p -> constTerm);
  Size psize = p -> psize;
  Term * terms = p -> terms;
  for (Size i = 0; i < psize; ++i)
    {
      terms[i].coeff = eval_convert_hs(convert, terms[i].coeff);
    }
}

Poly *
newConstPoly(const Coeff c, Var maxArity, Size maxSize)
{
  Poly * poly = (Poly *) malloc(sizeof(Poly));
  poly -> maxArity = maxArity;
  poly -> maxSize = maxSize;
  poly -> psize = 0;
  poly -> constTerm = c;
  poly -> terms = malloc(maxSize * sizeof(Term));

  // allocate space for terms' powers:
  for (Size i = 0; i < maxSize; i++)
    {
      (poly -> terms)[i].powers = (Power *) malloc(sizeof(Power) * maxArity);
      // no need to initialise powers and
      // coefficients as these terms are inactive
    }

  return poly;
}

Poly *
newProjectionPoly(const Coeff zero, const Coeff one, Var var, Var maxArity,
    Size maxSize)
{
  Poly * poly = newConstPoly(zero, maxArity, maxSize);

  // add one term for the variable:
  poly -> psize = 1;
  Term * term = poly -> terms;

  // initialise the term's coeff:
  term -> coeff = one;
  //  printf("newProjectionPoly: coeff one address = %p\n", term -> coeff);

  // initialise the term's powers:
  Power * powers = term -> powers;

  // all zero:
  for (Var i = 0; i < maxArity; ++i)
    {
      powers[i] = 0;
    }
  // except the chosen var:
  powers[var] = 1;

  return poly;
}

