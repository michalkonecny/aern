/*
 * GenericCoeff/bounds.c
 *
 *  Created on: 3 Feb 2011
 *     Authors: Jan Duracz, Michal Konecny
 */

#include <stdlib.h>
#include <stdio.h>

#include "GenericCoeff/coeff.h"
#include "GenericCoeff/poly.h"

//#define DEBUG_BOUNDS(x) x;
#define DEBUG_BOUNDS(x)

/*
 * overview of the algorithm used for boundsEnclUsingDer:
 *   compute estimates of all partial derivatives and check whether they definitely exclude zero
 *   for each variable whose partial derivative excludes zero, evaluate the polynomial for each domain endpoint
 *     thus the polynomial is evaluated 2^k times
 *       where k is the number of variables in which the poly is known to be monotone
 *   with each combination, encode the information on variables in an array where for each variable
 *     the value 0 means the polynomial may not be monotone in this variable,
 *     the value -1 or 1 means the polynomial is monotone in this variable and choose this endpoint for this evaluation
 */

void
ADD_COEFF_CODE(sumUpAbsCoeffs)(Ops * ops, CoeffMutable * res, Poly * p)
{
  DEBUG_BOUNDS(printf("sumUpAbsCoeffs: starting\n"));
  Term * terms = p -> terms;
  Size pSize = p -> psize;

  CoeffMutable temp = CFM_NEW(ops, CFM_ZERO(ops));

  int i = 0;
  while (i < pSize)
    {
      CFM_ABS_UP(ops, temp, terms[i].coeff); // temp := |terms[i].coeff|;
      CFM_ADD_UP(ops, * res, * res, temp); // res += temp;
      i++;
    }

  CFM_FREE(temp);
  DEBUG_BOUNDS(printf("sumUpAbsCoeffs: finishing\n"));
}

/*
 * Work out what is the sum of the abs values of non-constant terms
 * in the derivative of p by derVar as well as what is the constant
 * term of this derivative.  Round the sum upwards and safely bound the constant
 * term to facilitate a test whether the derivative avoids
 * crossing zero.
 *
 * The algorithm uses the derivative rule for Chebyshev polynomials:
 *
 * dT_n(x)/dx = n*( 2T_{n-1}(x) + 2T_{n-3}(x) + ... + 2T_1(x) ) for even n
 * dT_n(x)/dx = n*( 2T_{n-1}(x) + 2T_{n-3}(x) + ... + 1 ) for odd n
 */
void
ADD_COEFF_CODE(derivativeSumUpAbsCoeffsConst)(Ops * ops,
    CoeffMutable * resTerms, CoeffMutable * resConst,
    CoeffMutable * errorBound, Poly * p, Var derVar)
{
  DEBUG_BOUNDS(printf("derivativeSumUpAbsCoeffsConst: starting (derVar = %d)\n", derVar));

  Term * terms = p -> terms;
  Size pSize = p -> psize;

  CoeffMutable temp = CFM_NEW(ops, CFM_ZERO(ops));
  CoeffMutable temp2 = CFM_NEW(ops, CFM_ZERO(ops));

  CFM_ASSIGN_VAL(ops, *resTerms, CFM_ZERO(ops));
  CFM_ASSIGN_VAL(ops, *resConst, CFM_ZERO(ops));
  CFM_ASSIGN_VAL(ops, *errorBound, CFM_ZERO(ops));
  int i = 0;
  while (i < pSize)
    {
      DEBUG_BOUNDS(printf("derivativeSumUpAbsCoeffsConst: processing term %d\n", i));
      // check whether this term appears in the derivative and with what power of var:
      int pwr = POWER_OF_VAR(terms[i].powers, derVar);
      int deg = MONOMIAL_DEGREE(terms[i].powers);
      int multiple = pwr;
      if (pwr > 0)
        { // the term appears in the derivative

          // account for terms in the derivative without derVar:
          if (pwr & 1) // odd pwr
            {
              if (deg == pwr) // no other variables present
                { // the term DOES contribute to the contant term of the derivative
                  DEBUG_BOUNDS(printf("derivativeSumUpAbsCoeffsConst: term %d contributes to the constant term\n", i));

                  // add it to the constant term:
                  CFM_MULI_DN(ops, temp, terms[i].coeff, pwr); // temp = coeff *. pwr;
                  CFM_ADD_DN(ops, temp, * resConst, temp); // temp = resConst +. temp;

                  CFM_MULI_UP(ops, temp2, terms[i].coeff, pwr); // temp2 = coeff *^ pwr;
                  CFM_ADD_UP(ops, * resConst, * resConst, temp2); // resConst +^= temp2;

                  // work out the error and compensate for it:
                  CFM_SUB_UP(ops, temp, * resConst, temp2); // temp = resConst -^ temp;
                  CFM_ADD_UP(ops, *errorBound, *errorBound, temp); // errorBound +^= temp;

                  DEBUG_BOUNDS(printf("derivativeSumUpAbsCoeffsConst: constant term contribution accounted for\n"));
                }
              else
                { // the term does NOT contribute to the contant term of the derivative
                  DEBUG_BOUNDS(printf("derivativeSumUpAbsCoeffsConst: term %d does not contribute to the constant term\n", i));

                  // add its abs value to the term bound rounded upwards:
                  CFM_ABS_UP(ops, temp, terms[i].coeff); // temp := |terms[i].coeff|;
                  CFM_MULI_UP(ops, temp, temp, pwr); // temp *= pwr (the power derivative rule)
                  CFM_ADD_UP(ops, * resTerms, * resTerms, temp); // resTerms +^= temp;

                  DEBUG_BOUNDS(printf("derivativeSumUpAbsCoeffsConst: term without variable %d accounted for\n", derVar));
                }

              multiple *= (pwr - 1); // discount the constant term
            }
          else
            {
              multiple *= pwr;
            }

          DEBUG_BOUNDS(printf("derivativeSumUpAbsCoeffsConst: term %d contributing to terms with variable %d\n", i, derVar));
          // account for terms in the derivative where derVar has degree > 0:
          CFM_ABS_UP(ops, temp, terms[i].coeff); // temp := |terms[i].coeff|;
          CFM_MULI_UP(ops, temp, temp, multiple);
          // temp *= pwr*pwr or pwr*(pwr-1) - from the derivative rule for Chebyshev polynomials
          CFM_ADD_UP(ops, * resTerms, * resTerms, temp); // resTerms +^= temp;
          DEBUG_BOUNDS(printf("derivativeSumUpAbsCoeffsConst: term %d accounted for\n", i));
        }
      i++;
    }

  CFM_FREE(temp);
  DEBUG_BOUNDS(printf("derivativeSumUpAbsCoeffsConst: finishing\n"));
}

void
ADD_COEFF_CODE(evalDomainCombination)(Ops * ops, CoeffMutable * res,
    CoeffMutable * errorBound, Poly * p, int * domainCombination)
{
  DEBUG_BOUNDS(printf("evalDomainCombination: starting\n"));

  Term * terms = p -> terms;
  Size pSize = p -> psize;
  Var arity = p -> maxArity;

  CoeffMutable temp = CFM_NEW(ops, CFM_ZERO(ops));

  CFM_ASSIGN(ops, *res, p -> constTerm);
  int i = 0;
  while (i < pSize)
    {
      DEBUG_BOUNDS(printf("evalDomainCombination: evaluating term %d\n", i));

      // Check the term's powers to work out whether we can evaluate this term
      // at a corner or on the whole domain.
      // If there is a variable whose domainCombination value is 0, we cannot evaluate
      // over a corner:
      bool canDoCornerEval = true;
      FOREACH_VAR_ARITY(var, arity)
        {
          if (POWER_OF_VAR(terms[i].powers, var) > 0 && domainCombination[var]
              == 0)
            {
              canDoCornerEval = false;
              DEBUG_BOUNDS(printf("evalDomainCombination: cannot do corner evaluation for term %d due to variable %d\n", i, var));
              break;
            }
        }

      if (canDoCornerEval)
        {
          DEBUG_BOUNDS(printf("evalDomainCombination: doing corner evaluation for term %d\n", i));
          // work out the sign of the term's basis
          // using T_n(-1) = (-1)^n, T_n(1) = 1
          int termBasisSignPositive = true;
          FOREACH_VAR_ARITY(var, arity)
            {
              if (domainCombination[var] == -1)
                {
                  if (POWER_OF_VAR(terms[i].powers,var) & 1) // odd power
                    {
                      termBasisSignPositive = !termBasisSignPositive;
                    }
                }
            }

          if (termBasisSignPositive)
            { // add the coefficient to res:
              DEBUG_BOUNDS(printf("evalDomainCombination: term %d basis is positive\n", i));

              // alternate rounding up and down:
              if (i & 1) // if i is odd
                {
                  CFM_ADD_DN(ops, temp, *res, terms[i].coeff); // temp = res +. coeff;
                  CFM_ADD_UP(ops, *res, *res, terms[i].coeff); // res +^= coeff;
                  CFM_SUB_UP(ops, temp, *res, temp); // operation error estimate
                  CFM_ADD_UP(ops, *errorBound, *errorBound, temp);
                }
              else
                {
                  CFM_ADD_UP(ops, temp, *res, terms[i].coeff); // temp = res +^ coeff;
                  CFM_ADD_DN(ops, *res, *res, terms[i].coeff); // res +.= coeff;
                  CFM_SUB_UP(ops, temp, temp, *res); // operation error estimate
                  CFM_ADD_UP(ops, *errorBound, *errorBound, temp);
                }
            }
          else
            { // subtract the coefficient from res:
              DEBUG_BOUNDS(printf("evalDomainCombination: term %d basis is negative\n", i));

              // alternate rounding up and down:
              if (i & 1) // if i is odd
                {
                  CFM_SUB_DN(ops, temp, *res, terms[i].coeff); // temp = res -. coeff;
                  CFM_SUB_UP(ops, *res, *res, terms[i].coeff); // res -^= coeff;
                  CFM_SUB_UP(ops, temp, *res, temp); // operation error estimate
                  CFM_ADD_UP(ops, *errorBound, *errorBound, temp);
                }
              else
                {
                  CFM_SUB_UP(ops, temp, *res, terms[i].coeff); // temp = res -^ coeff;
                  CFM_SUB_DN(ops, *res, *res, terms[i].coeff); // res -.= coeff;
                  CFM_SUB_UP(ops, temp, temp, *res); // operation error estimate
                  CFM_ADD_UP(ops, *errorBound, *errorBound, temp);
                }
            }
        }
      else
        {
          DEBUG_BOUNDS(printf("evalDomainCombination: doing rough evaluation for term %d\n", i));
          // evaluate over the whole domain, ie [-1,1]*coeff,
          // thus |coeff| is added to the errorBound:
          CFM_ABS_UP(ops, temp, terms[i].coeff); // temp := |terms[i].coeff|;
          CFM_ADD_UP(ops, * errorBound, * errorBound, temp); // res += temp;
        }

      i++;
    }

  CFM_FREE(temp);
  DEBUG_BOUNDS(printf("evalDomainCombination: finishing\n"));
}

void
ADD_COEFF_CODE(boundUpUsingDer)(Ops * ops, CoeffMutable * res, Poly *p)
{
  DEBUG_BOUNDS(printf("boundUpUsingDer: starting; p =\n"));
  DEBUG_BOUNDS(ADD_COEFF_CODE(printPoly)(p));
  Var arity = p -> maxArity;

  // auxiliary variables:
  CoeffMutable temp = CFM_NEW(ops,CFM_ZERO(ops));
  CoeffMutable temp2 = CFM_NEW(ops,CFM_ZERO(ops));
  CoeffMutable temp3 = CFM_NEW(ops,CFM_ZERO(ops));

  // create an array to hold the monotonicity information about each variable:
  int * monotonicityInfo = malloc(arity * sizeof(int));
  bool detectedSomeMonotonicity = false;

  // evaluate all partial derivatives:
  FOREACH_VAR_ARITY(var, arity)
    {
      CoeffMutable derSumAbsNonConstCoeffs = temp;
      CoeffMutable derConstTerm = temp2;
      CoeffMutable derConstTermErrorBound = temp3;
      // like sumUpAbsCoeffs but for the derivative of p by var,
      // also giving the constant term of the derivative:
      ADD_COEFF_CODE(derivativeSumUpAbsCoeffsConst)(ops,
          &derSumAbsNonConstCoeffs, &derConstTerm, &derConstTermErrorBound, p,
          var);

      // check whether contant term is definitely not larger than the sum of other coeffs
      // (which implies that the derivative does not cross zero):
      CFM_ABS_DN(ops, derConstTerm, derConstTerm); // derConstTerm = |derConstTerm|
      CFM_SUB_DN(ops, derConstTerm, derConstTerm, derConstTermErrorBound); // derConstTerm -.= derConstTermErrorBound
      if (CFM_COMPARE(CFM_COMPARE_FN(ops), derSumAbsNonConstCoeffs, derConstTerm)
          == 1)
        {
          // too bad, we have not ruled out zero crossing:
          DEBUG_BOUNDS(printf("boundUpUsingDer: cannot ensure p monotone in variable %d\n", var));
          monotonicityInfo[var] = 0;
        }
      else
        {
          DEBUG_BOUNDS(printf("boundUpUsingDer: p is monotone in variable %d\n", var));
          monotonicityInfo[var] = 1;
          detectedSomeMonotonicity = true;
        }
    }

  if (!detectedSomeMonotonicity)
    {
      // cannot make use of monotonicity, do it the easy way:
      ADD_COEFF_CODE(boundUp)(ops, res, p);
    }
  else
    {

      // iterate through all domain combinations, collecting the maximum of all the results:
      CFM_ASSIGN(ops, *res, p -> constTerm); // certainly not above the upper bound
      int i = 0;
      bool carry = false;
      while (!carry)
        {
          DEBUG_BOUNDS(printf("boundUpUsingDer: processing face %d\n", i));
          CoeffMutable val = temp;
          CoeffMutable err = temp2;
          CFM_ASSIGN_VAL(ops, val, CFM_ZERO(ops));
          CFM_ASSIGN_VAL(ops, err, CFM_ZERO(ops));

          // evaluate with the current domain combination:
          ADD_COEFF_CODE(evalDomainCombination)(ops, &val, &err, p,
              monotonicityInfo);
          CFM_ADD_UP(ops, val, val, err); // rounding upwards

          // update the result:
          if (CFM_COMPARE(CFM_COMPARE_FN(ops), val, *res) == 1) // val > res
            {
              DEBUG_BOUNDS(printf("boundUpUsingDer: updating result with a higher value\n"));
              CFM_ASSIGN(ops, *res,val); // res = val
            }

          // "increment" monotonicity info in the "lexicographical" order:
          carry = true;
          FOREACH_VAR_ARITY(var, arity)
            {
              if (monotonicityInfo[var] == 0)
                {
                  continue;
                } // ignore variables

              if (monotonicityInfo[var] == 1)
                {
                  monotonicityInfo[var] = -1;
                  // completed the increment, no carry left:
                  carry = false;
                  break;
                }
              else
                {
                  monotonicityInfo[var] = 1;
                }
            }
          i++;
        }

      CFM_ADD_UP(ops, *res, *res, p -> errorBound);
    }
  FREE(monotonicityInfo);
  CFM_FREE(temp);
  CFM_FREE(temp2);
  CFM_FREE(temp3);
  DEBUG_BOUNDS(printf("boundUpUsingDer: finishing\n"));
}

void
ADD_COEFF_CODE(boundUpThin)(Ops * ops, CoeffMutable * res, Poly * p)
{
  CFM_ASSIGN(ops, *res, p -> constTerm);
  ADD_COEFF_CODE(sumUpAbsCoeffs)(ops, res, p);
}

void
ADD_COEFF_CODE(boundDnThin)(Ops * ops, CoeffMutable * res, Poly * p)
{
  CFM_ASSIGN(ops, *res, p -> constTerm);
  CFM_NEG(ops, *res, *res); // res := - constTerm
  ADD_COEFF_CODE(sumUpAbsCoeffs)(ops, res, p);
  CFM_NEG(ops, *res, *res); // res := - res
}

void
ADD_COEFF_CODE(boundUp)(Ops * ops, CoeffMutable * res, Poly * p)
{
  CFM_ASSIGN(ops, *res, p -> constTerm);
  CFM_ADD_UP(ops, *res, *res, p -> errorBound);
  ADD_COEFF_CODE(sumUpAbsCoeffs)(ops, res, p);
}

void
ADD_COEFF_CODE(boundDn)(Ops * ops, CoeffMutable * res, Poly * p)
{
  CFM_ASSIGN(ops, *res, p -> constTerm);
  CFM_SUB_DN(ops, *res, *res, p -> errorBound);
  CFM_NEG(ops, *res, *res); // res := - constTerm
  ADD_COEFF_CODE(sumUpAbsCoeffs)(ops, res, p);
  CFM_NEG(ops, *res, *res); // res := - res
}
