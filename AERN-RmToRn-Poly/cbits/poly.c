#include "poly.h"

#include <malloc.h>

Poly *
newConstPoly(Coeff c, Var maxArity, Size maxSize)
{
  void * poly = malloc(sizeof(Poly));
  poly.maxArity = maxArity;
  poly.maxSize = maxSize;
  poly.size = 0;
  poly.constTerm = c;
  poly.terms = malloc(maxSize * sizeof(Term));

  for (Size i = 0; i < maxSize; i++)
    {
      poly.terms[i].powers = malloc(sizeof(Power) * maxArity);
      // no need to initialise powers and
      // coefficients as these terms are inactive
    }

  return poly;
}

Poly *
newProjectionPoly(Ops * ops, Var var, Var maxArity, Size maxSize)
{
  void * poly = newConstPoly(ops.zero);
  poly.size = 1;
  Term * a = poly.terms[0];
}
