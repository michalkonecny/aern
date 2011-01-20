#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * This file should differ from its GenericCoeff analogue
 * only in the following include line and by omitting
 * the ...UsingMutableOps functions.
 */
#include "DoubleCoeff/poly.h"
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
  qsort(cns, size, sizeof(CoeffN), (__compar_fn_t)&ADD_COEFF_CODE(compareCoeffNsByN));
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
              memmove(terms[j].powers, terms1[newCoeffs[j].n1].powers, arity
                  * sizeof(Power));
            }
          else
            {
              // copy powers of variables from terms2:
              memmove(terms[j].powers, terms2[newCoeffs[j].n2].powers, arity
                  * sizeof(Power));
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
              memmove(terms[j].powers, terms1[newCoeffs[j].n1].powers, arity
                  * sizeof(Power));
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
                  memmove(terms[j].powers, terms1[newCoeffs[j].n1].powers,
                      arity * sizeof(Power));
                }
            }
          else
            {
              // copy powers of variables from terms2:
              memmove(terms[j].powers, terms2[newCoeffs[j].n2].powers, arity
                  * sizeof(Power));
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
              memmove(terms[j].powers, terms2[newCoeffs[j].n2].powers, arity
                  * sizeof(Power));
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
                  memmove(terms[j].powers, terms2[newCoeffs[j].n2].powers,
                      arity * sizeof(Power));
                }
            }
          else
            {
              // copy powers of variables from terms1:
              memmove(terms[j].powers, terms1[newCoeffs[j].n1].powers, arity
                  * sizeof(Power));
            }
        }
    }
}

Coeff
ADD_COEFF_CODE(addTermsAndReturnMaxError)(Coeff zero,
    const ComparisonOp compare, const Ops_Pure * ops, Poly *res,
    const Poly * p1, const Poly * p2)
{

  Var arity = res -> maxArity;
  Var maxSize = res -> maxSize;
  Term * terms = res -> terms;

  Term * terms1 = p1 -> terms;
  Term * terms2 = p2 -> terms;

  Size p1Size = p1 -> psize;
  Size p2Size = p2 -> psize;

  Coeff maxError = zero;

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
              powerComparison = memcmp(terms1[i1].powers, terms2[i2].powers,
                  arity * (sizeof(Power)));
            }

          Power degree;

          // fill in newCoeffs[i] and degree:
          if (powerComparison == 0)
            {
              //              printf(
              //                  "addTermsAndReturnMaxError: coeff %d: adding terms1[%d] and terms2[%d]\n",
              //                  i, i1, i2);
              // compute sum of the two coefficients and its error bound:
              Coeff newCfUp = CF_ADD_UP(ops, terms1[i1].coeff, terms2[i2].coeff);
              Coeff newCfDn = CF_ADD_DN(ops, terms1[i1].coeff, terms2[i2].coeff);
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
              degree = ADD_COEFF_CODE(getPowersDegree)(terms2[i2].powers, arity);
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
              degree = ADD_COEFF_CODE(getPowersDegree)(terms2[i2].powers, arity);
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
              degree = ADD_COEFF_CODE(getPowersDegree)(terms2[i1].powers, arity);
              i1++;
            }

          // check the term's degree is not above the limit:
          if(degree <= res -> maxDeg)
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

  //  printf("addTermsAndReturnMaxError: finished, returning maxError\n");

  return maxError;
}

void
ADD_COEFF_CODE(addUpUsingPureOps)(Coeff zero, ComparisonOp compare,
    const Ops_Pure * ops, Poly *res, const Poly * p1, const Poly * p2)
{
  Coeff maxError = ADD_COEFF_CODE(addTermsAndReturnMaxError)(zero, compare,
      ops, res, p1, p2);

  // compute the constant term coefficient rounding up:
  Coeff temp = CF_ADD_UP(ops, p1 -> constTerm, p2 -> constTerm);
  // also add maxError to the constant term coefficient:
  res -> constTerm = CF_ADD_UP(ops, temp, maxError);

  CF_FREE(temp);
  CF_FREE(maxError);
}

void
ADD_COEFF_CODE(addDnUsingPureOps)(Coeff zero, ComparisonOp compare,
    const Ops_Pure * ops, Poly *res, const Poly * p1, const Poly * p2)
{
  Coeff maxError = ADD_COEFF_CODE(addTermsAndReturnMaxError)(zero, compare,
      ops, res, p1, p2);

  // compute the constant term coefficient rounding down:
  Coeff temp = CF_ADD_DN(ops, p1 -> constTerm, p2 -> constTerm);
  // also subtract maxError from the constant term coefficient:
  res -> constTerm = CF_SUB_DN(ops, temp, maxError);

  CF_FREE(temp);
  CF_FREE(maxError);
}

//void
//testAssign(Coeff sample, UnaryOpMutable assign, CoeffMutable to,
//    CoeffMutable from)
//{
//  eval_unaryMutable_hs(sample, assign, to, from);
//}

