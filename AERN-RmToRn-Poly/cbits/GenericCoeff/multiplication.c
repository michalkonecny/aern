#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tree234.h>

#include "GenericCoeff/poly.h"
#include "EvalExport_stub.h"

// auxiliary structure and associated functions for multiplication:

typedef struct COEFF_PWR
{
  Power * powers;
  Var arity;
  CoeffMutable cf;
  ComparisonOp cfCompare;
} CoeffPowers;

int
ADD_COEFF_CODE(compareCoeffPowersByCoeffDecreasing)(const CoeffPowers * cn1,
    const CoeffPowers * cn2)
{
  return CFM_COMPARE(cn1 -> cfCompare, cn2 -> cf, cn1 -> cf);
}

int
ADD_COEFF_CODE(compareCoeffPowersByPowers)(const CoeffPowers * cn1,
    const CoeffPowers * cn2)
{
  return memcmp(cn1 -> powers, cn2 -> powers, SIZEOF_POWERS(cn1 -> arity));
}

void
ADD_COEFF_CODE(sortCoeffPowersByCoeffDecreasing)(int size, CoeffPowers * cns)
{
  qsort(cns, size, sizeof(CoeffPowers),
      (__compar_fn_t ) &ADD_COEFF_CODE(compareCoeffPowersByCoeffDecreasing));
}

void
ADD_COEFF_CODE(multiplyEncl)(Ops * ops, Poly *res, Poly * p1, Poly * p2)
{

  Var arity = res -> maxArity;
  Var maxSize = res -> maxSize;
  Var maxTermArity = res -> maxTermArity;
  Term * terms = res -> terms;

  // lookup important info from polys:
  Term * terms1 = p1 -> terms;
  Term * terms2 = p2 -> terms;

  Size p1Size = p1 -> psize;
  Size p2Size = p2 -> psize;

  // auxiliary variable:
  CoeffMutable temp = CFM_NEW(ops,CFM_ZERO(ops));

  // variable mirroring the result's error bound:
  CoeffMutable maxError = res -> errorBound;

  // combine the errorBounds from parameter polynomials
  // using the formula e = e1*e2 + e1*|p2| + e2*|p1|
  CFM_MUL_UP(ops, maxError, p1 -> errorBound, p2 -> errorBound);
  // add e2*p1:
  ADD_COEFF_CODE(boundUpThin)(ops, temp, p1);
  CFM_MUL_UP(ops, temp, temp, p2 -> errorBound);
  CFM_ADD_UP(ops, maxError, maxError, temp);
  // add e2*p1:
  ADD_COEFF_CODE(boundUpThin)(ops, temp, p2);
  CFM_MUL_UP(ops, temp, temp, p1 -> errorBound);
  CFM_ADD_UP(ops, maxError, maxError, temp);

  // check for the special cases of scaling:
  if (p1Size == 0) // p1 is constant
    {
      // copy p2 into res:
      ADD_COEFF_CODE(copyEncl)(ops, res, p2);
      // scale res by p1:
      ADD_COEFF_CODE(scaleEncl)(ops, p1 -> constTerm, res);
    }
  else if (p2Size == 0) // p2 is constant
    {
      // copy p1 into res:
      ADD_COEFF_CODE(copyEncl)(ops, res, p1);
      // scale res by p2:
      ADD_COEFF_CODE(scaleEncl)(ops, p2 -> constTerm, res);
    }
  else
    {
      // create a counted B-tree of CoeffPowers indexed by the array of powers sorted lexicographically:
      tree234 * newTerms = newtree234(
          &ADD_COEFF_CODE(compareCoeffPowersByPowers));

      // create an array to hold temporary CoeffPowers before they are merged into the tree:
      CoeffPowers * tempCoeffPowers =
           (CoeffPowers *) malloc(sizeof(CoeffPowers) * (1 << maxTermArity));
            // there are at most 2^maxTermArity different powers resulting from the
            // product of two Chebyshev terms (beware that arity sho

      // compute all term products and add them to the B-tree:
      // iterate through all pairs of terms from p1 and p2:
      for (int i1 = 0; i1 < p1Size; ++i1)
        {
          for (int i2 = 0; i2 < p2Size; ++i2)
            {
              // construct powers for all result terms:
              int combinations = 1;
              for(int var = 0; var < arity; ++var)
                {
                  //TODO
                  // check if var is shared in both terms:

                  // var shared, clone the combinations made so far and add both variants of var's power:
                  combinations <<= 1; // double combinations counter

                  // var only in p1, copy power from p1 into all combinations:

                  // var only in p2, copy power from p2 into all combinations:

                }

              // compute product of the two coefficients and its error bound:
              CoeffMutable newCfUp = CFM_NEW(ops, CFM_ZERO(ops));
              CFM_MUL_UP(ops, newCfUp, terms1[i1].coeff, terms2[i2].coeff);
              CoeffMutable newCfDn = CFM_NEW(ops, CFM_ZERO(ops));
              CFM_MUL_DN(ops, newCfDn, terms1[i1].coeff, terms2[i2].coeff);
              CoeffMutable newCoeff = newCfUp; // rename, as this is the new coeff to be used in the result

              CoeffMutable newCfMaxError = newCfDn; // (reuse the variable)
              CFM_SUB_UP(ops, newCfMaxError, newCfUp, newCfDn);

              // add the error bound to the accumulated error:
              CFM_ADD_UP(ops, maxError, maxError, newCfMaxError);

              // free temp variable:
              CFM_FREE(newCfMaxError);

            }
        }

    }
}
