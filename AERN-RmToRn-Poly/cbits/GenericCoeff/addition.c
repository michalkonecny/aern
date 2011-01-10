#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "poly.h"
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
compareCoeffNsByCoeffDecreasing(const CoeffN * cn1, const CoeffN * cn2)
{
  return eval_compare_hs(cn1 -> cfCompare, cn2 -> cf, cn1 -> cf);
}

int
compareCoeffNsByN(const CoeffN * cn1, const CoeffN * cn2)
{
  return (cn1 -> n - cn2 -> n);
}

void
sortCoeffNsByCoeffDecreasing(int size, CoeffN * cns)
{
  qsort(cns, size, sizeof(CoeffN), (__compar_fn_t)&compareCoeffNsByCoeffDecreasing);
}

void
sortCoeffNsByN(int size, CoeffN * cns)
{
  qsort(cns, size, sizeof(CoeffN), (__compar_fn_t)&compareCoeffNsByN);
}

void
copyTerms(CoeffN * newCoeffs, Size i, Var arity, Term * terms, Term * terms1,
    Term * terms2)
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
addTermsAndReturnMaxError(Coeff zero, const ComparisonOp compare,
    const Ops_Pure * ops, Poly *res, const Poly * p1, const Poly * p2)
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
          free_SP_hs(terms[j].coeff);
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
              powerComparison = memcmp(terms1[i1].powers, terms2[i2].powers,
                  arity * (sizeof(Power)));
            }

          if (powerComparison == 0)
            {
              //              printf(
              //                  "addTermsAndReturnMaxError: coeff %d: adding terms1[%d] and terms2[%d]\n",
              //                  i, i1, i2);
              // compute sum of the two coefficients and its error bound:
              Coeff newCfUp = eval_binary_hs(ops -> plusUp, terms1[i1].coeff,
                  terms2[i2].coeff);
              Coeff newCfDn = eval_binary_hs(ops -> plusDn, terms1[i1].coeff,
                  terms2[i2].coeff);
              Coeff newCfMaxError = eval_binary_hs(ops -> minusUp, newCfUp,
                  newCfDn);
              newCoeffs[i].cf = newCfUp;

              // add the error bound to the accumulated error:
              Coeff temp = maxError;
              maxError = eval_binary_hs(ops -> plusUp, maxError, newCfMaxError);

              // free temp numbers:
              free_SP_hs(temp);
              free_SP_hs(newCfDn);
              free_SP_hs(newCfMaxError);

              newCoeffs[i].n1 = i1;
              newCoeffs[i].n2 = i2;
              i1++;
              i2++;
            }
          else if (powerComparison > 0) // i2 is smaller
            {
              //              printf(
              //                  "addTermsAndReturnMaxError: coeff %d: copying terms2[%d]\n",
              //                  i, i2);
              newCoeffs[i].cf = clone_SP_hs(terms2[i2].coeff);
              newCoeffs[i].n1 = -1;
              newCoeffs[i].n2 = i2;
              i2++;
            }
          else if (powerComparison < 0) // i1 is smaller
            {
              //              printf(
              //                  "addTermsAndReturnMaxError: coeff %d: copying terms1[%d]\n",
              //                  i, i1);
              newCoeffs[i].cf = clone_SP_hs(terms1[i1].coeff);
              newCoeffs[i].n1 = i1;
              newCoeffs[i].n2 = -1;
              i1++;
            }

          i++;
        }

      // i now holds the number of new coefficients,
      // check whether they fit into res:
      if (i > maxSize)
        {
          //          printf("addTermsAndReturnMaxError: reducing number of coeffs from %d to %d.\n", i, maxSize);
          // need to reduce the size, sort them from largest to smallest:
          sortCoeffNsByCoeffDecreasing(i, newCoeffs);
          // now sort the first maxSize coeffs by N, ie the power order:
          sortCoeffNsByN(maxSize, newCoeffs);
          // first maxSize coeffs to be used with their terms,
          // the remaining coeffs' absolute values are added to the constant term:
          for (int j = maxSize; j < i; ++j)
            {
              Coeff coeffAbs = eval_unary_hs(ops -> absUp, newCoeffs[j].cf);
              Coeff temp = maxError;
              maxError = eval_binary_hs(ops -> plusUp, maxError, coeffAbs);

              free_SP_hs(temp);
              free_SP_hs(coeffAbs);
              free_SP_hs(newCoeffs[j].cf);
            }

          // from now on, pretend that there are only maxSize terms:
          i = maxSize;
        }

      // free previous term coefficients in res:
      for (int j = 0; j < res -> psize; ++j)
        {
          free_SP_hs(terms[j].coeff);
        }

      // set the actual term size of the result:
      res -> psize = i;

      //      printf("addTermsAndReturnMaxError: about to construct %d resulting term(s)\n", i);
      // construct the resulting terms:
      copyTerms(newCoeffs, i, arity, terms, terms1, terms2);

      free(newCoeffs);
    }

  //  printf("addTermsAndReturnMaxError: finished, returning maxError\n");

  return maxError;
}

void
addUpUsingPureOps(Coeff zero, const ComparisonOp compare, const Ops_Pure * ops,
    const Ops_Mutable * _opsM, Poly *res, const Poly * p1, const Poly * p2)
{
  Coeff maxError = addTermsAndReturnMaxError(zero, compare, ops, res, p1, p2);

  // compute the constant term coefficient rounding up:
  Coeff temp = eval_binary_hs(ops -> plusUp, p1 -> constTerm, p2 -> constTerm);
  // also add maxError to the constant term coefficient:
  res -> constTerm = eval_binary_hs(ops -> plusUp, temp, maxError);

  free_SP_hs(temp);
  free_SP_hs(maxError);
}

void
addDnUsingPureOps(Coeff zero, const ComparisonOp compare, const Ops_Pure * ops,
    const Ops_Mutable * _opsM, Poly *res, const Poly * p1, const Poly * p2)
{
  Coeff maxError = addTermsAndReturnMaxError(zero, compare, ops, res, p1, p2);

  // compute the constant term coefficient rounding down:
  Coeff temp = eval_binary_hs(ops -> plusDn, p1 -> constTerm, p2 -> constTerm);
  // also subtract maxError from the constant term coefficient:
  res -> constTerm = eval_binary_hs(ops -> minusDn, temp, maxError);
  free(temp);
  free(maxError);
}

void
testAssign(Coeff sample, UnaryOpMutable assign, CoeffMutable to,
    CoeffMutable from)
{
  eval_unaryMutable_hs(sample, assign, to, from);
}

void
addTermsAndReturnMaxErrorUsingMutableOps(Coeff sample,
    const ComparisonOp compare, const Ops_Mutable * opsM, Poly *res,
    const Poly * p1, const Poly * p2, CoeffMutable maxError)
{

  Var arity = res -> maxArity;
  Var maxSize = res -> maxSize;
  Term * terms = res -> terms;

  // lookup important info from polys:
  Term * terms1 = p1 -> terms;
  Term * terms2 = p2 -> terms;

  Size p1Size = p1 -> psize;
  Size p2Size = p2 -> psize;

  //  printf("addTermsAndReturnMaxErrorMutable: p1Size = %d \n", p1Size);
  //  printf("addTermsAndReturnMaxErrorMutable: p2Size = %d \n", p2Size);


  if (p1Size + p2Size == 0) // no need to compute any terms
    {
      // free previous term coefficients in res:
      for (int j = 0; j < res -> psize; ++j)
        {
          free_SP_hs(terms[j].coeff);
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
              powerComparison = memcmp(terms1[i1].powers, terms2[i2].powers,
                  arity * (sizeof(Power)));
            }

          if (powerComparison == 0)
            {
              //              printf(
              //                  "addTermsAndReturnMaxErrorMutable: coeff %d: adding terms1[%d] and terms2[%d]\n",
              //                  i, i1, i2);
              // compute sum of the two coefficients and its error bound:
              CoeffMutable newCfUp = eval_newMutable_hs(opsM -> new, sample);
              eval_binaryMutable_hs(sample, opsM -> plusUpMutable, newCfUp,
                  terms1[i1].coeff, terms2[i2].coeff);
              CoeffMutable newCfDn = eval_newMutable_hs(opsM -> new, sample);
              eval_binaryMutable_hs(sample, opsM -> plusDnMutable, newCfDn,
                  terms1[i1].coeff, terms2[i2].coeff);
              newCoeffs[i].cf = newCfUp;

              CoeffMutable newCfMaxError = newCfDn; // reuse the variable
              eval_binaryMutable_hs(sample, opsM -> minusUpMutable,
                  newCfMaxError, newCfUp, newCfDn);

              // add the error bound to the accumulated error:
              eval_binaryMutable_hs(sample, opsM -> plusUpMutable, maxError,
                  maxError, newCfMaxError);

              // free temp variable:
              free_SP_hs(newCfMaxError);

              newCoeffs[i].n1 = i1;
              newCoeffs[i].n2 = i2;
              i1++;
              i2++;
            }
          else if (powerComparison > 0) // i2 is smaller
            {
              //              printf(
              //                  "addTermsAndReturnMaxError: coeff %d: copying terms2[%d]\n",
              //                  i, i2);
              newCoeffs[i].cf = eval_cloneMutable_hs(sample, opsM -> clone,
                  terms2[i2].coeff);
              newCoeffs[i].n1 = -1;
              newCoeffs[i].n2 = i2;
              i2++;
            }
          else if (powerComparison < 0) // i1 is smaller
            {
              //              printf(
              //                  "addTermsAndReturnMaxError: coeff %d: copying terms1[%d]\n",
              //                  i, i1);
              newCoeffs[i].cf = eval_cloneMutable_hs(sample, opsM -> clone,
                  terms1[i1].coeff);
              newCoeffs[i].n1 = i1;
              newCoeffs[i].n2 = -1;
              i1++;
            }

          i++;
        }

      // i now holds the number of new coefficients,
      // check whether they fit into res:
      if (i > maxSize)
        {
          //          printf("addTermsAndReturnMaxError: reducing number of coeffs from %d to %d.\n", i, maxSize);
          // need to reduce the size, sort them from largest to smallest:
          sortCoeffNsByCoeffDecreasing(i, newCoeffs);
          // now sort the first maxSize coeffs by N, ie the power order:
          sortCoeffNsByN(maxSize, newCoeffs);
          // first maxSize coeffs to be used with their terms,
          // the remaining coeffs' absolute values are added to the constant term:
          for (int j = maxSize; j < i; ++j)
            {
              eval_unaryMutable_hs(sample, opsM -> absUpMutable,
                  newCoeffs[j].cf, newCoeffs[j].cf);
              eval_binaryMutable_hs(sample, opsM -> plusUpMutable, maxError,
                  maxError, newCoeffs[j].cf);
              free_SP_hs(newCoeffs[j].cf);
            }

          // from now on, pretend that there are only maxSize terms:
          i = maxSize;
        }

      // free previous term coefficients in res:
      for (int j = 0; j < res -> psize; ++j)
        {
          free_SP_hs(terms[j].coeff);
        }

      // set the new term size of the result:
      res -> psize = i;

      //      printf("addTermsAndReturnMaxError: about to construct %d resulting term(s)\n", i);
      copyTerms(newCoeffs, i, arity, terms, terms1, terms2);

      free(newCoeffs);
    }

  //  printf("addTermsAndReturnMaxError: finished\n");
}

void
addUpUsingMutableOps(Coeff zero, const ComparisonOp compare,
    const Ops_Pure * opsP, const Ops_Mutable * opsM, Poly *res,
    const Poly * p1, const Poly * p2)
{
  Coeff sample = zero;

  CoeffMutable maxError = eval_newMutable_hs(opsM -> new, zero);
  addTermsAndReturnMaxErrorUsingMutableOps(zero, compare, opsM, res, p1, p2,
      maxError);

  // compute the constant term coefficient rounding up:
  eval_binaryMutable_hs(sample, opsM -> plusUpMutable, res -> constTerm,
      p1 -> constTerm, p2 -> constTerm);
  // also add maxError to the constant term coefficient:
  eval_binaryMutable_hs(sample, opsM -> plusUpMutable, res -> constTerm,
      res -> constTerm, maxError);

  free_SP_hs(maxError);
}

void
addDnUsingMutableOps(Coeff zero, const ComparisonOp compare,
    const Ops_Pure * opsP, const Ops_Mutable * opsM, Poly *res,
    const Poly * p1, const Poly * p2)
{
  Coeff sample = zero;

  CoeffMutable maxError = eval_newMutable_hs(opsM -> new, zero);
  addTermsAndReturnMaxErrorUsingMutableOps(zero, compare, opsM, res, p1, p2,
      maxError);

  // compute the constant term coefficient rounding down:
  eval_binaryMutable_hs(sample, opsM -> plusDnMutable, res -> constTerm,
      p1 -> constTerm, p2 -> constTerm);
  // also subtract maxError from the constant term coefficient:
  eval_binaryMutable_hs(sample, opsM -> minusDnMutable, res -> constTerm,
      res -> constTerm, maxError);

  free_SP_hs(maxError);
}

