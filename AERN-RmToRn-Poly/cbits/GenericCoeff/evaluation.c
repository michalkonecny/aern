#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "GenericCoeff/poly.h"
#include "EvalExport_stub.h"

#define DEBUG_EVAL(x) x;
//#define DEBUG_EVAL(x)

Value
ADD_COEFF_CODE(evalAtPtChebBasis)(Poly * p, Value * values, Value one,
    BinaryOp add, BinaryOp subtr, BinaryOp mult, ConversionOp cf2val)
{
  DEBUG_EVAL(printf("evalAtPtChebBasis: starting\n"));
  Var maxArity = p -> maxArity;
  Var psize = p -> psize;
  Term * terms = p -> terms;

  // initialise the result variable using the constant term:
  DEBUG_EVAL(printf("evalAtPtChebBasis: converting the constant term\n"));
  Value result = eval_convert_hs(cf2val, p -> constTerm);
  DEBUG_EVAL(printf("evalAtPtChebBasis: converted the constant term\n"));

  // return if this is a constant polynomial:
  if (psize == 0)
    {
      return result;
    }

  // work out maximum power for each variable:
  Power * maxPowers = calloc(maxArity, sizeof(Power));
  for (Size termNo = 0; termNo < psize; ++termNo)
    {
      Power * powers = terms[termNo].powers;
      FOREACH_VAR_ARITY(var, maxArity)
        {
          if (POWER_OF_VAR(powers,var) > maxPowers[var])
            {
              maxPowers[var] = POWER_OF_VAR(powers,var);
              //              printf("powers[%d]=%d\n", var, powers[var]);
            }
        }
    }
  DEBUG_EVAL(printf("evalAtPtChebBasis: computed power limits for all variables\n"));

  // compute Chebyshev powers for all values up to the variable's maximum power:
  Value ** varPowers = malloc(maxArity * sizeof(Value *));
  FOREACH_VAR_ARITY(var, maxArity)
    {
      Power varMaxN = maxPowers[var];
      DEBUG_EVAL(printf("evalAtPtChebBasis: computing powers of var %d up to %d\n", var, varMaxN));

      Value x = values[var];


      // allocate space for powers of x using the maximum N:
      varPowers[var] = malloc((1 + varMaxN) * sizeof(Value));

      // initialise powers 0 and 1 if these are required:
      if (varMaxN >= 0)
        {
          varPowers[var][0] = one;
        }
      if (varMaxN >= 1)
        {
          varPowers[var][1] = x;
        }

      // compute powers 2 and onwards using the iterative formula T_n(x) = 2*x*T_{n-1}(x) - T_{n-2}(x)
      for (Power n = 2; n <= varMaxN; ++n)
        {
          Value temp1 = eval_binary_hs(mult, x, varPowers[var][n - 1]);
          Value temp2 = eval_binary_hs(add, temp1, temp1);
          varPowers[var][n] = eval_binary_hs(subtr, temp2,
              varPowers[var][n - 2]);
          free_SP_hs(temp1);
          free_SP_hs(temp2);
        }
    }
  DEBUG_EVAL(printf("evalAtPtChebBasis: computed value powers for all variables\n"));

  // scan the polynomial terms, cumulatively computing the result:
  for (Size termNo = 0; termNo < psize; ++termNo)
    {
      // printf("evalAtPtPowerBasis: processing term x^%d y^%d\n", terms[termNo].powers[0], terms[termNo].powers[1]);
      // printf("evalAtPtPowerBasis: coeff addr = %p\n", terms[termNo].coeff);
      Value termValue = eval_convert_hs(cf2val, terms[termNo].coeff);

      FOREACH_VAR_ARITY(var, maxArity)
        {
          Power pwr = POWER_OF_VAR(terms[termNo].powers,var);
          if (pwr > 0)
            {
              //              printf(" multiplying for var %d with power %d\n", var, pwr);
              Value temp = termValue;
              termValue = eval_binary_hs(mult, termValue, varPowers[var][pwr]);
              free_SP_hs(temp);
            }
        }

      Value temp = result;
      result = eval_binary_hs(add, result, termValue);
      free_SP_hs(temp);
      free_SP_hs(termValue);
    }
  DEBUG_EVAL(printf("evalAtPtChebBasis: computed the polynomial's value\n"));

  // free pre-computed Chebyshev powers:
  FOREACH_VAR_ARITY(var, maxArity)
    {
      // free the haskell values pointed to by this array:
      for (Power power = 2; power < maxPowers[var]; ++power)
        {
          free_SP_hs(varPowers[var][power]);
        };
      // free the array of pointers to power values:
      free(varPowers[var]);
    }
  // free the array of pointers to arrays that point to power values:
  free(varPowers);
  // free the array with maximum powers for each variable:
  free(maxPowers);

  DEBUG_EVAL(printf("evalAtPtChebBasis: finished, returning result\n"));
  return result;
}


Value
ADD_COEFF_CODE(evalAtPtPowerBasis)(Poly * p, Value * values, Value one,
    BinaryOp add, BinaryOp mult, ConversionOp cf2val)
{
//  printf("evalAtPtPowerBasis: starting\n");
  Var maxArity = p -> maxArity;
  Var psize = p -> psize;
  Term * terms = p -> terms;

  // initialise the result variable using the constant term:
  Value result = eval_convert_hs(cf2val, p -> constTerm);

  // return if this is a constant polynomial:
  if (psize == 0)
    {
      return result;
    }

  // work out maximum power for each variable:
  Power * maxPowers = calloc(maxArity, sizeof(Power));
  for (Size termNo = 0; termNo < psize; ++termNo)
    {
      Power * powers = terms[termNo].powers;
      for (int var = 0; var < maxArity; ++var)
        {
          if (POWER_OF_VAR(powers,var) > maxPowers[var])
            {
              maxPowers[var] = POWER_OF_VAR(powers,var);
              //              printf("powers[%d]=%d\n", var, powers[var]);
            }
        }
    }

  // compute powers for all values up to the variable's maximum power:
  Value ** varPowers = malloc(maxArity * sizeof(Value *));
  for (Var var = 0; var < maxArity; ++var)
    {
      Power varMaxN = maxPowers[var];
      Value x = values[var];

      // allocate space for powers of x using the maximum N:
      varPowers[var] = malloc((1 + varMaxN) * sizeof(Value));

      // initialise powers 0 and 1 if these are required:
      if (varMaxN >= 0)
        {
          varPowers[var][0] = one;
        }
      if (varMaxN >= 1)
        {
          varPowers[var][1] = x;
        }

      // compute powers 2 and onwards from lower powers:
      for (Power n = 2; n <= varMaxN; ++n)
        {
          /*
           * for even n: x^n = x^(n/2) * x^(n/2)
           * for odd n: x^n = x^((n-1)/2) * x^((n+1)/2)
           */
          Value temp1 = varPowers[var][n/2]; // for odd n, n/2 = (n-1)/2
          Value temp2 = varPowers[var][(n+1)/2]; // for even n, (n+1)/2 = n/2
          varPowers[var][n] = eval_binary_hs(mult, temp1, temp2);
        }
    }

  // scan the polynomial terms, cumulatively computing the result:
  for (Size termNo = 0; termNo < psize; ++termNo)
    {
      // printf("evalAtPtPowerBasis: processing term x^%d y^%d\n", terms[termNo].powers[0], terms[termNo].powers[1]);
      // printf("evalAtPtPowerBasis: coeff addr = %p\n", terms[termNo].coeff);
      Value termValue = eval_convert_hs(cf2val, terms[termNo].coeff);

      for (Var var = 0; var < maxArity; ++var)
        {
          Power pwr = POWER_OF_VAR(terms[termNo].powers,var);
          if (pwr > 0)
            {
              //              printf(" multiplying for var %d with power %d\n", var, pwr);
              Value temp = termValue;
              termValue = eval_binary_hs(mult, termValue, varPowers[var][pwr]);
              free_SP_hs(temp);
            }
        }

      Value temp = result;
      result = eval_binary_hs(add, result, termValue);
      free_SP_hs(temp);
      free_SP_hs(termValue);
    }

  // free pre-computed powers:
  for (Var var = 0; var < maxArity; ++var)
    {
      // free the haskell values pointed to by this array:
      for (Power power = 2; power < maxPowers[var]; ++power)
        {
          free_SP_hs(varPowers[var][power]);
        };
      // free the array of pointers to power values:
      free(varPowers[var]);
    }

  // free the array of pointers to arrays that point to power values:
  free(varPowers);

  // free the array with maximum powers for each variable:
  free(maxPowers);

//  printf("evalAtPtPowerBasis: returning\n");
  return result;
}
