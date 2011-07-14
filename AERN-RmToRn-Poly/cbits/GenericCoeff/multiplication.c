#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tree234.h>

#include "GenericCoeff/poly.h"
#include "EvalExport_stub.h"

//#define DEBUG_MULT(x) x;
#define DEBUG_MULT(x)

/*
 * overview copied from the to-do list:
 *
    CoeffPower - like addition's CoeffN but holds an array of powers instead of N
    create a counted B-tree of CoeffPowers indexed by the array of powers sorted lexicographically
    compute all term products and add them to the B-tree
      remember that T_i(x)*T_j(x) = T_{i+j}(x)/2 + T_{|i-j|}(x)/2
        thus a product of two Chebyshev terms that share n variables
          contributes to 2^n different Chebyshev terms in the result
      when adding a term, first check whether there is a term with such power
        if so, add the new coeff to the coeff of the existing term and update maxError
      detect contributions to the constant term and collate them separately
    if there are too many terms, index them sorted by coeff size in decreasing order
    mark all CoeffPowers that are to be copied to the result
    copy the constant term separately
    iterate through the terms in the B-tree in the power order, following the constant term
      if a term is marked, copy it to the result (reusing the power array)
      otherwise adjust maxError and free the power array
      in any case free each CoeffPower structure after processing
    free the B-tree
    set the constant/error term
 */

// auxiliary structure and associated functions for multiplication:

typedef struct COEFF_PWR
{
  Power * powers;
  Var arity;
  CoeffMutable cf;
  CoeffMutable abs_cf;
  ComparisonOp cfCompare;
} CoeffPowers;

int
ADD_COEFF_CODE(compareCoeffPowersByAbsCoeffDecreasing)(const CoeffPowers ** cn1,
    const CoeffPowers ** cn2)
{
  DEBUG_MULT(printf("compareCoeffPowersByCoeffDecreasing: comparing cn1 = %p cn2 = %p\n", *cn1, *cn2));
  return CFM_COMPARE((*cn1) -> cfCompare, (*cn2) -> abs_cf, (*cn1) -> abs_cf);
}

int
ADD_COEFF_CODE(compareCoeffPowersByPowers)(const CoeffPowers * cn1,
    const CoeffPowers * cn2)
{
  return memcmp(cn1 -> powers, cn2 -> powers, SIZEOF_POWERS(cn1 -> arity));
}

void
ADD_COEFF_CODE(sortCoeffPowersByAbsCoeffDecreasing)(int size, CoeffPowers ** cns)
{
  qsort(cns, size, sizeof(CoeffPowers *),
      (__compar_fn_t ) &ADD_COEFF_CODE(compareCoeffPowersByAbsCoeffDecreasing));
}

void
ADD_COEFF_CODE(multiplyTermsAndConsts)(Ops * ops, Poly *res, Poly * p1,
    Poly * p2)
{

  Size p1Size = p1 -> psize;
  Size p2Size = p2 -> psize;

  DEBUG_MULT(printf("multiplyTermsAndConsts: p1Size = %d, p2Size = %d\n", p1Size, p2Size));

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
      Var arity = res -> maxArity;
      Var maxSize = res -> maxSize;
      Var maxDeg = res -> maxDeg;
      Var maxTermArity = res -> maxTermArity;
      Term * terms = res -> terms;

      // lookup important info from polys:
      Term * terms1 = p1 -> terms;
      Term * terms2 = p2 -> terms;

      // auxiliary variables:
      CoeffMutable temp = CFM_NEW(ops,CFM_ZERO(ops));
      CoeffMutable temp2 = CFM_NEW(ops,CFM_ZERO(ops));
      DEBUG_MULT(printf("multiplyTermsAndConsts: creating temp = %p, temp2 = %p\n", temp, temp2));

      // variable mirroring the result's error bound:
      CoeffMutable maxError = res -> errorBound;

      DEBUG_MULT(printf("multiplyTermsAndConsts: creating newTerms\n"));

      // create a counted B-tree of CoeffPowers indexed by the array of powers sorted lexicographically:
      tree234 * newTerms = newtree234(
          (cmpfn234) &ADD_COEFF_CODE(compareCoeffPowersByPowers));

      DEBUG_MULT(printf("multiplyTermsAndConsts: creating tempCoeffPowers of size %d.\n", 1
              << maxTermArity));
      // create an array to hold temporary CoeffPowers before they are merged into the tree:
      CoeffPowers ** tempCoeffPowers = (CoeffPowers **) malloc(
          sizeof(CoeffPowers *) * (1 << maxTermArity));
      DEBUG_MULT(printf("multiplyTermsAndConsts: created tempCoeffPowers\n"));

      // there are at most 2^maxTermArity different powers resulting from the
      // product of two Chebyshev terms

      // compute all term products and add them to the B-tree:
      // iterate through all pairs of terms from p1 and p2 and construct new CoeffPowers:
      for (Size i1 = 0; i1 <= p1Size; ++i1) // 0 is the constant term
        {
          for (Size i2 = 0; i2 <= p2Size; ++i2) // 0 is the constant term
            {
              DEBUG_MULT(printf("multiplyTermsAndConsts: looking at terms no %d and %d.\n", i1, i2));
              // construct CoeffPowers for all result terms,
              // part-initialising first CoeffPowers structure:
              int combinations = 1;
              tempCoeffPowers[0] = malloc(sizeof(CoeffPowers));
              tempCoeffPowers[0] -> arity = arity;
              tempCoeffPowers[0] -> cfCompare = CFM_COMPARE_FN(ops);
              tempCoeffPowers[0] -> cf = CFM_NEW(ops, CFM_ZERO(ops));
              tempCoeffPowers[0] -> powers = (Power *) malloc(
                  SIZEOF_POWERS(arity));

              DEBUG_MULT(printf("multiplyTermsAndConsts: initialised first CoeffPowers (cf = %p).\n", tempCoeffPowers[0] -> cf));

              CoeffMutable cf1 = i1 == 0 ? p1 -> constTerm
                  : terms1[i1 - 1].coeff;
              CoeffMutable cf2 = i2 == 0 ? p2 -> constTerm
                  : terms2[i2 - 1].coeff;

              // compute all combinations of powers for the result of multiplying these two terms:
              FOREACH_VAR_ARITY(var, arity)
                {
                  Power power1 = i1 == 0 ? 0
                      : POWER_OF_VAR(terms1[i1 - 1].powers, var);
                  Power power2 = i2 == 0 ? 0
                      : POWER_OF_VAR(terms2[i2 - 1].powers, var);

                  DEBUG_MULT(printf("multiplyTermsAndConsts: var %d has powers %d and %d in p1 and p2, respectively.\n", var, power1, power2));

                  // check if var is shared in both terms:
                  if (power1 > 0 && power2 > 0)
                    {
                      // use the formula for multiplication of Chebyshev polynomials
                      // to work out the power of var in the result term:
                      Power newPower1 = power1 + power2;
                      Power newPower2 = abs(power1 - power2);
                      // clone all combinations made so far, add the first variant into the original
                      // and the second variant into the cloned combinations:
                      for (int c = 0, cc = combinations; c < combinations; ++c, ++cc)
                        {
                          POWER_OF_VAR(tempCoeffPowers[c] -> powers, var)
                              = newPower1;
                          // clone the CoeffPowers structure:
                          tempCoeffPowers[cc] = malloc(sizeof(CoeffPowers));
                          memcpy(tempCoeffPowers[cc], tempCoeffPowers[c],
                              sizeof(CoeffPowers));
                          // decouple the cf variables:
                          tempCoeffPowers[cc] -> cf
                              = CFM_NEW(ops, CFM_ZERO(ops));
                          DEBUG_MULT(printf("multiplyTermsAndConsts: created combination %d (cf = %p)\n", cc, tempCoeffPowers[cc] -> cf););

                          // clone the powers array of this combination to the cloned combination (deepening the clone):
                          tempCoeffPowers[cc] -> powers = (Power *) malloc(
                              SIZEOF_POWERS(arity));
                          memcpy(tempCoeffPowers[cc] -> powers,
                              tempCoeffPowers[c] -> powers, SIZEOF_POWERS(var));
                          // before changing the cloned combination so that it uses the second variant:
                          POWER_OF_VAR(tempCoeffPowers[cc] -> powers, var)
                              = newPower2;
                        }

                      combinations <<= 1; // double the combinations counter
                    }
                  else if (power1 > 0)
                    {
                      // var only in p1, copy power from p1 into all combinations:
                      for (int c = 0; c < combinations; ++c)
                        {
                          POWER_OF_VAR(tempCoeffPowers[c] -> powers, var)
                              = power1;
                        }
                    }
                  else
                    {
                      // var only in p2, copy power from p2 into all combinations:
                      for (int c = 0; c < combinations; ++c)
                        {
                          POWER_OF_VAR(tempCoeffPowers[c] -> powers, var)
                              = power2;
                        }
                    }
                }

              DEBUG_MULT(printf("multiplyTermsAndConsts: constructed power combinations; count = %d.\n", combinations));

              for (int c = 0; c < combinations; ++c)
                {
                  // complete the degree and term arity info in this powers array:
                  int dg = 0;
                  int termArity = 0;
                  FOREACH_VAR_ARITY(var, arity)
                    {
                      int pwr = POWER_OF_VAR(tempCoeffPowers[c] -> powers, var);
                      dg += pwr;
                      if (pwr > 0)
                        {
                          termArity++;
                        }
                    }
                  MONOMIAL_DEGREE(tempCoeffPowers[c] -> powers) = dg;
                  TERM_ARITY(tempCoeffPowers[c] -> powers) = termArity;

                  DEBUG_MULT(printf("multiplyTermsAndConsts: combination %d: term degree = %d, term arity = %d\n", c, dg, termArity));
                }

              // compute product of the two coefficients rounded up and down:
              CoeffMutable newCfUp = temp;
              CFM_MUL_UP(ops, newCfUp, cf1, cf2);
              CoeffMutable newCfDn = temp2;
              CFM_MUL_DN(ops, newCfDn, cf1, cf2);
              DEBUG_MULT(printf("multiplyTermsAndConsts: computed coeff product\n"));

              // divide the product by combinations (ie 2^sharedVarCount):
              CFM_DIVI_UP(ops,newCfUp, newCfUp, combinations);
              CFM_DIVI_DN(ops,newCfDn, newCfDn, combinations);
              DEBUG_MULT(printf("multiplyTermsAndConsts: divided it by %d\n", combinations));

              CoeffMutable newCoeff = newCfUp; // rename, as this is the new coeff to be used in the result

              // compute a bound on the error incurred in computing newCoeff:
              CoeffMutable newCfMaxError = newCfDn; // (reuse the variable)
              CFM_SUB_UP(ops, newCfMaxError, newCfUp, newCfDn);

              // multiply newCfMaxError by the number of added terms (ie combinations):
              CFM_MULI_UP(ops, newCfMaxError, newCfMaxError, combinations);

              // add the error bound to the accumulated error:
              CFM_ADD_UP(ops, maxError, maxError, newCfMaxError);
              // value of temp2 no longer needed


              // put the new coeff into all created result terms:
              for (int c = 0; c < combinations; ++c)
                {
                  CFM_ASSIGN(ops, tempCoeffPowers[c] -> cf, newCoeff);
                }
              // value of temp no longer needed

              // integrate the temporary CoeffPowers that meet
              // the term-specific criteria into the tree,
              // freeing any powers arrays that are not integrated:
              for (int c = 0; c < combinations; ++c)
                {
                  CoeffPowers * newCP = tempCoeffPowers[c];

                  // check whether the term qualifies for addition:
                  if (TERM_ARITY(newCP -> powers) > maxTermArity
                      || MONOMIAL_DEGREE(newCP -> powers) > maxDeg)
                    {
                      DEBUG_MULT(printf("multiplyTermsAndConsts: combination %d exceeds limits, removing and compensating\n", c));
                      // term exceeds the limits, ignore it and compensate for it in maxError:
                      CFM_ABS_UP(ops, newCP -> cf, newCP -> cf);
                      CFM_ADD_UP(ops, maxError, maxError, newCP -> cf);

                      // free the term:
                      free(newCP -> powers);
                      CFM_FREE(newCP -> cf);
                      free(newCP);
                    }
                  else
                    {
                      DEBUG_MULT(printf("multiplyTermsAndConsts: trying to add combination %d\n", c));
                      // try adding the CoeffPowers to the tree:
                      CoeffPowers * oldCP = add234(newTerms, newCP);

                      if (oldCP != newCP)
                        {
                          DEBUG_MULT(printf("multiplyTermsAndConsts: combination %d's powers already present in the tree, adding coeff %p to coeff %p\n", c, newCP -> cf, oldCP -> cf));
                          // another CoeffPowers with the same powers array content is already there;
                          // sum up the new coeff and the old coeff and
                          // update the coeff in the old CoeffPowers with the result:
                          CoeffMutable sumUp = temp;
                          CFM_ADD_UP(ops, sumUp, oldCP -> cf, newCP -> cf);
                          CFM_ADD_DN(ops, oldCP -> cf, oldCP -> cf, newCP -> cf);

                          // bound the error:
                          CoeffMutable sumError = temp2;
                          CFM_SUB_UP(ops, sumError, sumUp, oldCP -> cf);
                          // value of temp no longer needed

                          // add the error to the tally:
                          CFM_ADD_UP(ops, maxError, maxError, sumError);
                          // value of temp2 no longer needed

                          // free the newCP:
                          free(newCP -> powers);
                          CFM_FREE(newCP -> cf);
                          free(newCP);
                        }
                    }
                }

            } // end of for loop iterating over terms of p2
        } // end of for loop iterating over terms of p1


      DEBUG_MULT(printf("multiplyTermsAndConsts: about to free tempCoeffPowers\n"));
      // free temp CoeffPowers:
      free(tempCoeffPowers);

      // determine whether a further reduction is needed due to maxSize:
      int newTermsSize = count234(newTerms) - 1; // do not count the constant term
      DEBUG_MULT(printf("multiplyTermsAndConsts: created %d non-constant terms\n", newTermsSize));

      if (newTermsSize > maxSize)
        {
          DEBUG_MULT(printf("multiplyTermsAndConsts: need to reduce down to %d terms\n", maxSize));

          // initialise an array via which the new terms will be sorted by coeff size:
          CoeffPowers ** newTermsArray = (CoeffPowers **) malloc(
              sizeof(CoeffPowers *) * newTermsSize);
          DEBUG_MULT(printf("multiplyTermsAndConsts: created newTermsArray of size %d \n", newTermsSize));

          int newTermsArraySize = -1;
          // add all non-constant terms into newTermsArray in powers order and complete their abs_cf:
          for (CoeffPowers * t = NULL; (t = findrel234(newTerms, t, NULL,
              REL234_GT)) != NULL;)
            {
              if (newTermsArraySize >= 0)
                {
                  DEBUG_MULT(printf("multiplyTermsAndConsts: adding term %d to newTermsArray  \n", newTermsArraySize));
                  t -> abs_cf = CFM_NEW(ops, CFM_ZERO(ops));
                  CFM_ABS_UP(ops, t -> abs_cf, t -> cf);
                  newTermsArray[newTermsArraySize] = t;
                  DEBUG_MULT(printf("multiplyTermsAndConsts: term %d at %p has cf = %p abs_cf = %p\n", newTermsArraySize, t, t -> cf, t -> abs_cf));
                }
              newTermsArraySize++;
            }

          DEBUG_MULT(printf("multiplyTermsAndConsts: sorting newTermsArray (size = %d) \n", newTermsArraySize));
          // sort newTerms by decreasing abs(cf):
          ADD_COEFF_CODE(sortCoeffPowersByAbsCoeffDecreasing)(newTermsArraySize,
              newTermsArray);
          DEBUG_MULT(printf("multiplyTermsAndConsts: finished sorting\n"));

          // remove the terms with smallest coeffs:
          for (int i = maxSize; i < newTermsArraySize; i++)
            {
              CoeffPowers * t = newTermsArray[i];
              del234(newTerms, t);

              // compensate for the term:
              CFM_ADD_UP(ops, maxError, maxError, t -> abs_cf);

              // free the term:
              free(t -> powers);
              CFM_FREE(t -> cf);
              CFM_FREE(t -> abs_cf);
              free(t);
            }

          // deallocate the abs value variables:
          for (int i = 0; i < maxSize; ++i)
            {
              CFM_FREE(newTermsArray[i] -> abs_cf);
            }

          free(newTermsArray); // no longer needed

          // update the size count:
          newTermsSize = maxSize;
        }

      // move remaining contents of newTerms into res:
      Size oldResPsize = res -> psize;
      res -> psize = newTermsSize;

      for (int i = -1; i < newTermsSize; ++i)
        {
          // take the least element off the tree:
          CoeffPowers * t = delpos234(newTerms, 0);
          if (i < 0)
            {
              DEBUG_MULT(printf("multiplyTermsAndConsts: about to copy constant term to res\n"));
              CFM_FREE(res -> constTerm);
              res -> constTerm = t -> cf;
              free(t -> powers);
              DEBUG_MULT(printf("multiplyTermsAndConsts: copied constant term to res\n"));
            }
          else
            {
              DEBUG_MULT(printf("multiplyTermsAndConsts: about to copy term %d to res\n", i));
              if (i < oldResPsize)
                {
                  CFM_FREE(terms[i].coeff);
                }
              terms[i].coeff = t -> cf;
              free(terms[i].powers);
              terms[i].powers = t -> powers;
              DEBUG_MULT(printf("multiplyTermsAndConsts: copied term %d to res\n", i));
            }
          // free the rest of t:
          free(t);
        }

      // having freed all its contents, free also the newTerms tree:
      freetree234(newTerms);
      DEBUG_MULT(printf("multiplyTermsAndConsts: freed the newTerms tree\n"));

      CFM_FREE(temp);
      CFM_FREE(temp2);
      // set the errorBound of result:
      CFM_ASSIGN(ops, res -> errorBound, maxError);

    } // end of case where both p1 and p2 have some terms
}

void
ADD_COEFF_CODE(multiplyUp)(Ops * ops, Poly *res, Poly * p1, Poly * p2)
{
  DEBUG_MULT(printf("multiplyUp: starting\n"));
  DEBUG_MULT(printf("multiplyUp: p1 = \n"));
  DEBUG_MULT(ADD_COEFF_CODE(printPoly)(p1));
  DEBUG_MULT(printf("multiplyUp: p2 = \n"));
  DEBUG_MULT(ADD_COEFF_CODE(printPoly)(p2));

  ADD_COEFF_CODE(multiplyTermsAndConsts)(ops, res, p1, p2);
  DEBUG_MULT(printf("performed main multiplication\n"));

  // add the errorBound to the constant term coefficient:
  CFM_ADD_UP(ops, res -> constTerm, res -> constTerm, res -> errorBound);

  // set the error bound to zero:
  CFM_ASSIGN_VAL(ops, res -> errorBound, CFM_ZERO(ops));
  DEBUG_MULT(printf("multiplyUp: finished\n"));
  DEBUG_MULT(printf("multiplyUp: result = \n"));
  DEBUG_MULT(ADD_COEFF_CODE(printPoly)(res));

}

void
ADD_COEFF_CODE(multiplyDn)(Ops * ops, Poly *res, Poly * p1, Poly * p2)
{
  //  printf("multiplyDn: starting\n");

  ADD_COEFF_CODE(multiplyTermsAndConsts)(ops, res, p1, p2);
  //  printf("performed main multiplication\n");

  // subtract the errorBound from the constant term coefficient:
  CFM_SUB_DN(ops, res -> constTerm, res -> constTerm, res -> errorBound);

  // set the error bound to zero:
  CFM_ASSIGN_VAL(ops, res -> errorBound, CFM_ZERO(ops));
  //  printf("multiplyDn: finished\n");
}

void
ADD_COEFF_CODE(multiplyEncl)(Ops * ops, Poly *res, Poly * p1, Poly * p2)
{
  ADD_COEFF_CODE(multiplyTermsAndConsts)(ops, res, p1, p2);

  CoeffMutable temp = CFM_NEW(ops, CFM_ZERO(ops));

  // add to res -> errorBound the radius derived from the radii of p1 and p2:

  // the derived radius is estimated
  // using the formula e = e1*e2 + e1*|p2| + e2*|p1|
  CFM_MUL_UP(ops, temp, p1 -> errorBound, p2 -> errorBound);
  CFM_ADD_UP(ops, res -> errorBound, res -> errorBound, temp);
  // add e2*p1:
  ADD_COEFF_CODE(boundUpThin)(ops, temp, p1);
  CFM_MUL_UP(ops, temp, temp, p2 -> errorBound);
  CFM_ADD_UP(ops, res -> errorBound, res -> errorBound, temp);
  // add e2*p1:
  ADD_COEFF_CODE(boundUpThin)(ops, temp, p2);
  CFM_MUL_UP(ops, temp, temp, p1 -> errorBound);
  CFM_ADD_UP(ops, res -> errorBound, res -> errorBound, temp);
  // value of temp no longer needed

  CFM_FREE(temp);
}

