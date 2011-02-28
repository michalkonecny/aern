#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "GenericCoeff/poly.h"
#include "EvalExport_stub.h"

// auxiliary structure and associated functions for addition:

typedef struct COEFFN
{
  Coeff cf;
  ComparisonOp cfCompare;
  int n;
  int n1;
  int n2;
} CoeffN;

int
ADD_COEFF_CODE(compareCoeffNsByCoeffDecreasing)(const CoeffN * cn1,
    const CoeffN * cn2)
{
  return CF_COMPARE(cn1 -> cfCompare, cn2 -> cf, cn1 -> cf);
}

int
ADD_COEFF_CODE(compareCoeffNsByN)(const CoeffN * cn1, const CoeffN * cn2)
{
  return (cn1 -> n - cn2 -> n);
}

void
ADD_COEFF_CODE(sortCoeffNsByCoeffDecreasing)(int size, CoeffN * cns)
{
  qsort(cns, size, sizeof(CoeffN),
      (__compar_fn_t ) &ADD_COEFF_CODE(compareCoeffNsByCoeffDecreasing));
}

void
ADD_COEFF_CODE(sortCoeffNsByN)(int size, CoeffN * cns)
{
  qsort(cns, size, sizeof(CoeffN),
      (__compar_fn_t ) &ADD_COEFF_CODE(compareCoeffNsByN));
}

void
ADD_COEFF_CODE(copyTerms)(CoeffN * newCoeffs, Size i, Var arity, Term * terms,
    Term * terms1, Term * terms2)
{
  if (terms != terms1 && terms != terms2)// no aliasing with result
    {
      for (int j = 0; j < i; ++j)
        {
          // set the newly computed coefficient:
          terms[j].coeff = newCoeffs[j].cf;

          // copy the powers from terms1 or terms2:
          if (newCoeffs[j].n1 >= 0)
            {
              // copy powers of variables from terms1:
              memmove(terms[j].powers, terms1[newCoeffs[j].n1].powers, SIZEOF_POWERS(arity));
            }
          else
            {
              // copy powers of variables from terms2:
              memmove(terms[j].powers, terms2[newCoeffs[j].n2].powers, SIZEOF_POWERS(arity));
            }

        }
    }
  else if (terms1 == terms)// aliasing option1
    {
      // pass 1 left to right from term1:
      for (int j = 0; j < i; ++j)
        {
          // set the newly computed coefficient:
          terms[j].coeff = newCoeffs[j].cf;

          if (newCoeffs[j].n1 >= 0 && newCoeffs[j].n1 > j)
            {
              // copy powers of variables from terms1 if they are moving to the left:
              memmove(terms[j].powers, terms1[newCoeffs[j].n1].powers, SIZEOF_POWERS(arity));
            }
        }
      // pass 2 right to left from term1 or term2:
      for (int j = i - 1; j >= 0; --j)
        {
          if (newCoeffs[j].n1 >= 0)
            {
              // copy the powers from term1 if they are NOT moving to the left:
              if (newCoeffs[j].n1 <= j)
                {
                  // copy powers of variables from terms1:
                  memmove(terms[j].powers, terms1[newCoeffs[j].n1].powers, SIZEOF_POWERS(arity));
                }
            }
          else
            {
              // copy powers of variables from terms2:
              memmove(terms[j].powers, terms2[newCoeffs[j].n2].powers, SIZEOF_POWERS(arity));
            }
        }
    }
  else if (terms2 == terms)// aliasing option1
    {
      // pass 1 left to right from term2:
      for (int j = 0; j < i; ++j)
        {
          // set the newly computed coefficient:
          terms[j].coeff = newCoeffs[j].cf;

          if (newCoeffs[j].n2 >= 0 && newCoeffs[j].n2 > j)
            {
              // copy powers of variables from terms2 if they are moving to the left:
              memmove(terms[j].powers, terms2[newCoeffs[j].n2].powers, SIZEOF_POWERS(arity));
            }
        }
      // pass 2 right to left from term1 or term2:
      for (int j = i - 1; j >= 0; --j)
        {
          if (newCoeffs[j].n2 >= 0)
            {
              // copy the powers from terms2 if they are NOT moving to the left:
              if (newCoeffs[j].n2 <= j)
                {
                  // copy powers of variables from terms1:
                  memmove(terms[j].powers, terms2[newCoeffs[j].n2].powers, SIZEOF_POWERS(arity));
                }
            }
          else
            {
              // copy powers of variables from terms1:
              memmove(terms[j].powers, terms1[newCoeffs[j].n1].powers, SIZEOF_POWERS(arity));
            }
        }
    }
}


void
ADD_COEFF_CODE(addTermsAndErrorBoundsUsingMutableOps)(ComparisonOp compare,
    Ops_Mutable * opsM, Poly *res, Poly * p1, Poly * p2)
{

  Var arity = res -> maxArity;
  Var maxSize = res -> maxSize;
  Term * terms = res -> terms;

  // lookup important info from polys:
  Term * terms1 = p1 -> terms;
  Term * terms2 = p2 -> terms;

  Size p1Size = p1 -> psize;
  Size p2Size = p2 -> psize;

  // create an easy to refer to alias
  // of the mutable variable for result's error bounds:
  CoeffMutable maxError = res -> errorBound;

  // combine the errorBounds from parameter polynomials:
  CFM_ADD_UP(opsM, maxError, p1 -> errorBound, p2 -> errorBound);

  if (p1Size + p2Size == 0) // no need to compute any terms
    {
      // free previous term coefficients in res:
      for (int j = 0; j < res -> psize; ++j)
        {
          CFM_FREE(terms[j].coeff);
        }

      res -> psize = 0;
    }
  else
    {
      CoeffN * newCoeffs = malloc((p1Size + p2Size) * sizeof(CoeffN));

      int i = 0;
      int i1 = 0;
      int i2 = 0;

      //      printf("addTermsAndReturnMaxErrorMutable: about to compute coeffs\n");
      // compute new coefficients in the order of increasing powers:
      while (i1 < p1Size || i2 < p2Size)
        {
          //          printf(
          //              "addTermsAndReturnMaxError: about to compute coeff for i = %d, i1 = %d, i2 = %d;\n",
          //              i, i1, i2);
          newCoeffs[i].n = i;
          newCoeffs[i].cfCompare = compare;

          // work out which polynomial(s) to read the next term from:
          int powerComparison;
          if (i1 == p1Size)
            {
              powerComparison = 1;
            } // only p2 has some terms left
          else if (i2 == p2Size)
            {
              powerComparison = -1;
            } // only p1 has some terms left
          else
            {
              powerComparison = memcmp(terms1[i1].powers, terms2[i2].powers, SIZEOF_POWERS(arity));
            }

          Power degree;

          // fill in newCoeffs[i] and degree:
          if (powerComparison == 0)
            {
              //              printf(
              //                  "addTermsAndReturnMaxErrorMutable: coeff %d: adding terms1[%d] and terms2[%d]\n",
              //                  i, i1, i2);
              // compute sum of the two coefficients and its error bound:
              CoeffMutable newCfUp = CFM_NEW(opsM, CFM_SAMPLE(opsM));
              CFM_ADD_UP(opsM, newCfUp, terms1[i1].coeff, terms2[i2].coeff);
              CoeffMutable newCfDn = CFM_NEW(opsM, CFM_SAMPLE(opsM));
              CFM_ADD_DN(opsM, newCfDn, terms1[i1].coeff, terms2[i2].coeff);
              newCoeffs[i].cf = newCfUp;

              CoeffMutable newCfMaxError = newCfDn; // reuse the variable
              CFM_SUB_UP(opsM, newCfMaxError, newCfUp, newCfDn);

              // add the error bound to the accumulated error:
              CFM_ADD_UP(opsM, maxError, maxError, newCfMaxError);

              // free temp variable:
              CFM_FREE(newCfMaxError);

              newCoeffs[i].n1 = i1;
              newCoeffs[i].n2 = i2;
              degree = MONOMIAL_DEGREE(terms1[i1].powers);
              i1++;
              i2++;
            }
          else if (powerComparison > 0) // i2 is smaller
            {
              //              printf(
              //                  "addTermsAndReturnMaxError: coeff %d: copying terms2[%d]\n",
              //                  i, i2);
              CFM_CLONE(opsM, newCoeffs[i].cf, terms2[i2].coeff);
              newCoeffs[i].n1 = -1;
              newCoeffs[i].n2 = i2;
              degree = MONOMIAL_DEGREE(terms2[i2].powers);
              i2++;
            }
          else if (powerComparison < 0) // i1 is smaller
            {
              //              printf(
              //                  "addTermsAndReturnMaxError: coeff %d: copying terms1[%d]\n",
              //                  i, i1);
              CFM_CLONE(opsM, newCoeffs[i].cf, terms1[i1].coeff);
              newCoeffs[i].n1 = i1;
              newCoeffs[i].n2 = -1;
              degree = MONOMIAL_DEGREE(terms1[i1].powers);
              i1++;
            }

          // check the term's degree is not above the limit:
          if (degree <= res -> maxDeg)
            {
              // finalise the new term by increasing the term counter:
              i++;
            }
          else
            {
              // ignore the term by not increasing i
              // instead, add the absolute value of its coefficient to maxError:
              CFM_ABS_UP(opsM, newCoeffs[i].cf, newCoeffs[i].cf);
              CFM_ADD_UP(opsM, maxError, maxError, newCoeffs[i].cf);

              // and tidy up:
              CFM_FREE(newCoeffs[i].cf);
            }
        }

      // i now holds the number of new coefficients,
      // check whether they fit into res:
      if (i > maxSize)
        {
          //          printf("addTermsAndReturnMaxError: reducing number of coeffs from %d to %d.\n", i, maxSize);
          // need to reduce the size, sort them from largest to smallest:
          ADD_COEFF_CODE(sortCoeffNsByCoeffDecreasing)(i, newCoeffs);
          // now sort the first maxSize coeffs by N, ie the power order:
          ADD_COEFF_CODE(sortCoeffNsByN)(maxSize, newCoeffs);
          // first maxSize coeffs to be used with their terms,
          // the remaining coeffs' absolute values are added to the constant term:
          for (int j = maxSize; j < i; ++j)
            {
              CFM_ABS_UP(opsM, newCoeffs[j].cf, newCoeffs[j].cf);
              CFM_ADD_UP(opsM, maxError, maxError, newCoeffs[j].cf);
              CFM_FREE(newCoeffs[j].cf);
            }

          // from now on, pretend that there are only maxSize terms:
          i = maxSize;
        }

      // free previous term coefficients in res:
      for (int j = 0; j < res -> psize; ++j)
        {
          CFM_FREE(terms[j].coeff);
        }

      // set the new term size of the result:
      res -> psize = i;

      //      printf("addTermsAndReturnMaxError: about to construct %d resulting term(s)\n", i);
      ADD_COEFF_CODE(copyTerms)(newCoeffs, i, arity, terms, terms1, terms2);

      free(newCoeffs);
    }

  //  printf("addTermsAndReturnMaxError: finished\n");
}

void
ADD_COEFF_CODE(addUpUsingMutableOps)(Coeff zero, ComparisonOp compare,
    Ops_Mutable * opsM, Poly *res, Poly * p1, Poly * p2)
{
  //  printf("addUpUsingMutableOps: starting\n");

  ADD_COEFF_CODE(addTermsAndErrorBoundsUsingMutableOps)(compare, opsM, res, p1,
      p2);
  //  printf("performed main addition\n");

  // compute the constant term coefficient rounding up:
  CFM_ADD_UP(opsM, res -> constTerm, p1 -> constTerm, p2 -> constTerm);
  // also add the errorBound to the constant term coefficient:
  CFM_ADD_UP(opsM, res -> constTerm, res -> constTerm, res -> errorBound);

  CFM_ASSIGN_VAL(opsM, res -> errorBound, zero);
  //  printf("addUpUsingMutableOps: finished\n");
}

void
ADD_COEFF_CODE(addDnUsingMutableOps)(Coeff zero, ComparisonOp compare,
    Ops_Mutable * opsM, Poly *res, Poly * p1, Poly * p2)
{
  //  printf("addUpUsingMutableOps: starting\n");

  ADD_COEFF_CODE(addTermsAndErrorBoundsUsingMutableOps)(compare, opsM, res, p1,
      p2);
  //  printf("performed main addition\n");

  // compute the constant term coefficient rounding down:
  CFM_ADD_DN(opsM, res -> constTerm, p1 -> constTerm, p2 -> constTerm);
  // also subtract the errorBound from the constant term coefficient:
  CFM_SUB_DN(opsM, res -> constTerm, res -> constTerm, res -> errorBound);

  CFM_ASSIGN_VAL(opsM, res -> errorBound, zero);
  //  printf("addUpUsingMutableOps: finished\n");
}

void
ADD_COEFF_CODE(addEnclUsingMutableOps)(ComparisonOp compare,
    Ops_Mutable * opsM, Poly *res, Poly * p1, Poly * p2)
{
  ADD_COEFF_CODE(addTermsAndErrorBoundsUsingMutableOps)(compare, opsM, res, p1,
      p2);

  // compute the constant term coefficient rounding up and down:
  CFM_ADD_UP(opsM, res -> constTerm, p1 -> constTerm, p2 -> constTerm);

  CoeffMutable temp = CFM_NEW(opsM, CFM_SAMPLE(opsM));
  CFM_ADD_DN(opsM, temp, p1 -> constTerm, p2 -> constTerm);

  // get a bound on the rounding error:
  CFM_SUB_UP(opsM, temp, res -> constTerm, temp);

  // add this to the overall errorBound:
  CFM_ADD_UP(opsM, res -> errorBound, res -> errorBound, temp);
  CFM_FREE(temp);
}

/**
 * Perform addition of all terms except the constant terms.
 * Also set the errorBound in res to be the combined errorBounds
 * of p1 and p2 plus an upper bound on the accumulated error
 * of the addition.
 */
void
ADD_COEFF_CODE(addTermsAndErrorBoundsUsingPureOps)(ComparisonOp compare, Ops_Pure * ops,
    Poly *res, Poly * p1, Poly * p2)
{

  Var arity = res -> maxArity;
  Var maxSize = res -> maxSize;
  Term * terms = res -> terms;

  Term * terms1 = p1 -> terms;
  Term * terms2 = p2 -> terms;

  Size p1Size = p1 -> psize;
  Size p2Size = p2 -> psize;

  Coeff maxError = CF_ADD_UP(ops, p1 -> errorBound, p2 -> errorBound);

  //  printf("addTermsAndReturnMaxError: p1Size = %d \n", p1Size);
  //  printf("addTermsAndReturnMaxError: p2Size = %d \n", p2Size);

  if (p1Size + p2Size == 0) // no need to compute any terms?
    {
      // free previous term coefficients in res:
      for (int j = 0; j < res -> psize; ++j)
        {
          CF_FREE(terms[j].coeff);
        }

      res -> psize = 0;
    }
  else
    {
      CoeffN * newCoeffs = malloc((p1Size + p2Size) * sizeof(CoeffN));

      int i = 0;
      int i1 = 0;
      int i2 = 0;

      //      printf("addTermsAndReturnMaxError: about to compute coeffs\n");
      // compute new coefficients in the order of lexicographically increasing powers:
      while (i1 < p1Size || i2 < p2Size)
        {
          //          printf(
          //              "addTermsAndReturnMaxError: about to compute coeff for i = %d, i1 = %d, i2 = %d;\n",
          //              i, i1, i2);
          newCoeffs[i].n = i;
          newCoeffs[i].cfCompare = compare;

          // work out which polynomial(s) to read newCoeffs[i] from:
          int powerComparison;
          if (i1 == p1Size)
            {
              powerComparison = 1;
            } // only p2 has some terms left
          else if (i2 == p2Size)
            {
              powerComparison = -1;
            } // only p1 has some terms left
          else
            {
              powerComparison = memcmp(terms1[i1].powers, terms2[i2].powers, SIZEOF_POWERS(arity));
            }

          Power degree;

          // fill in newCoeffs[i] and degree:
          if (powerComparison == 0)
            {
              //              printf(
              //                  "addTermsAndReturnMaxError: coeff %d: adding terms1[%d] and terms2[%d]\n",
              //                  i, i1, i2);
              // compute sum of the two coefficients and its error bound:
              Coeff newCfUp =
                  CF_ADD_UP(ops, terms1[i1].coeff, terms2[i2].coeff);
              Coeff newCfDn =
                  CF_ADD_DN(ops, terms1[i1].coeff, terms2[i2].coeff);
              Coeff newCfMaxError = CF_SUB_UP(ops, newCfUp, newCfDn);
              newCoeffs[i].cf = newCfUp;

              // add the error bound to the accumulated error:
              Coeff temp = maxError;
              maxError = CF_ADD_UP(ops, maxError, newCfMaxError);

              // free temp numbers:
              CF_FREE(temp);
              CF_FREE(newCfDn);
              CF_FREE(newCfMaxError);

              newCoeffs[i].n1 = i1;
              newCoeffs[i].n2 = i2;
              degree = MONOMIAL_DEGREE(terms1[i1].powers);
              i1++;
              i2++;
            }
          else if (powerComparison > 0) // i2 is smaller
            {
              //              printf(
              //                  "addTermsAndReturnMaxError: coeff %d: copying terms2[%d]\n",
              //                  i, i2);
              newCoeffs[i].cf = CF_CLONE(terms2[i2].coeff);
              newCoeffs[i].n1 = -1;
              newCoeffs[i].n2 = i2;
              degree = MONOMIAL_DEGREE(terms2[i2].powers);
              i2++;
            }
          else if (powerComparison < 0) // i1 is smaller
            {
              //              printf(
              //                  "addTermsAndReturnMaxError: coeff %d: copying terms1[%d]\n",
              //                  i, i1);
              newCoeffs[i].cf = CF_CLONE(terms1[i1].coeff);
              newCoeffs[i].n1 = i1;
              newCoeffs[i].n2 = -1;
              degree = MONOMIAL_DEGREE(terms1[i1].powers);
              i1++;
            }

          // check the term's degree is not above the limit:
          if (degree <= res -> maxDeg)
            {
              // finalise the new term by increasing the term counter:
              i++;
            }
          else
            {
              // ignore the term by not increasing i
              // instead, add the absolute value of its coefficient to maxError:
              Coeff temp1 = CF_ABS_UP(ops, newCoeffs[i].cf);
              Coeff temp2 = maxError;
              maxError = CF_ADD_UP(ops, maxError, temp1);

              // and tidy up:
              CF_FREE(newCoeffs[i].cf);
              CF_FREE(temp1);
              CF_FREE(temp2);
            }
        }

      // i now holds the number of new coefficients,
      // check whether they fit into res:
      if (i > maxSize)
        {
          //          printf("addTermsAndReturnMaxError: reducing number of coeffs from %d to %d.\n", i, maxSize);
          // need to reduce the size, sort them from largest to smallest:
          ADD_COEFF_CODE(sortCoeffNsByCoeffDecreasing)(i, newCoeffs);
          // now sort the first maxSize coeffs by N, ie the power order:
          ADD_COEFF_CODE(sortCoeffNsByN)(maxSize, newCoeffs);
          // first maxSize coeffs to be used with their terms,
          // the remaining coeffs' absolute values are added to the constant term:
          for (int j = maxSize; j < i; ++j)
            {
              Coeff coeffAbs = CF_ABS_UP(ops, newCoeffs[j].cf);
              Coeff temp = maxError;
              maxError = CF_ADD_UP(ops, maxError, coeffAbs);

              CF_FREE(temp);
              CF_FREE(coeffAbs);
              CF_FREE(newCoeffs[j].cf);
            }

          // from now on, pretend that there are only maxSize terms:
          i = maxSize;
        }

      // free previous term coefficients in res:
      for (int j = 0; j < res -> psize; ++j)
        {
          CF_FREE(terms[j].coeff);
        }

      // set the actual term size of the result:
      res -> psize = i;

      //      printf("addTermsAndReturnMaxError: about to construct %d resulting term(s)\n", i);
      // construct the resulting terms:
      ADD_COEFF_CODE(copyTerms)(newCoeffs, i, arity, terms, terms1, terms2);

      free(newCoeffs);

    }

  // set the errorBound in res to be the inherited and accumulated error:
  CF_FREE(res -> errorBound);
  res -> errorBound = maxError;
  //  printf("addTermsAndReturnMaxError: finished\n");
}

void
ADD_COEFF_CODE(addUpUsingPureOps)(Coeff zero, ComparisonOp compare,
    Ops_Pure * ops, Poly *res, Poly * p1, Poly * p2)
{
  ADD_COEFF_CODE(addTermsAndErrorBoundsUsingPureOps)(compare, ops, res, p1, p2);

  // compute the constant term coefficient rounding up:
  Coeff temp = CF_ADD_UP(ops, p1 -> constTerm, p2 -> constTerm);

  // add errorBound to the constant term coefficient:
  res -> constTerm = CF_ADD_UP(ops, temp, res -> errorBound);
  CF_FREE(temp);

  // make this a thin enclosure, ie a simple polynomial:
  CF_FREE(res -> errorBound);
  res -> errorBound = zero;
}

void
ADD_COEFF_CODE(addDnUsingPureOps)(Coeff zero, ComparisonOp compare,
    Ops_Pure * ops, Poly *res, Poly * p1, Poly * p2)
{
  ADD_COEFF_CODE(addTermsAndErrorBoundsUsingPureOps)(compare, ops, res, p1, p2);

  // compute the constant term coefficient rounding down:
  Coeff temp = CF_ADD_DN(ops, p1 -> constTerm, p2 -> constTerm);

  // subtract errorBound from the constant term coefficient:
  res -> constTerm = CF_SUB_DN(ops, temp, res -> errorBound);
  CF_FREE(temp);

  // make this a thin enclosure, ie a simple polynomial:
  CF_FREE(res -> errorBound);
  res -> errorBound = zero;
}

void
ADD_COEFF_CODE(addEnclUsingPureOps)(ComparisonOp compare, Ops_Pure * ops,
    Poly *res, Poly * p1, Poly * p2)
{
  ADD_COEFF_CODE(addTermsAndErrorBoundsUsingPureOps)(compare, ops, res, p1, p2);

  // compute the constant term coefficient rounding up and down:
  res -> constTerm = CF_ADD_UP(ops, p1 -> constTerm, p2 -> constTerm);
  Coeff temp = CF_ADD_DN(ops, p1 -> constTerm, p2 -> constTerm);

  // work out a bound on the error made in the constant term:
  Coeff constTermErr = CF_SUB_UP(ops, res -> constTerm, temp);
  CF_FREE(temp);

  // update the radius of the enclosure:
  temp = res -> errorBound;
  res -> errorBound = CF_ADD_UP(ops, temp, constTermErr);
  CF_FREE(temp);
  CF_FREE(constTermErr);
}
