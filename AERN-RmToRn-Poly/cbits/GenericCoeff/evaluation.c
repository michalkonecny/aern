#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "GenericCoeff/poly.h"
#include "EvalExport_stub.h"

Value
ADD_COEFF_CODE(evalAtPtChebBasis)(const Poly * p, const Value * values, const Value one,
    const BinaryOp add, const BinaryOp subtr, const BinaryOp mult,
    const ConversionOp cf2val)
{
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
          if (powers[var] > maxPowers[var])
            {
              maxPowers[var] = powers[var];
              //              printf("powers[%d]=%d\n", var, powers[var]);
            }
        }
    }

  // compute Chebyshev powers for all values up to the variable's maximum power:
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

      // compute powers 2 and onwards using the iterative formula T_n(x) = 2*x*T_{n-1}(x) - T_{n-2}(x)
      for (Power n = 2; n < varMaxN; ++n)
        {
          Value temp1 = eval_binary_hs(mult, x, varPowers[var][n - 1]);
          Value temp2 = eval_binary_hs(add, temp1, temp1);
          varPowers[var][n] = eval_binary_hs(subtr, temp2,
              varPowers[var][n - 2]);
          free_SP_hs(temp1);
          free_SP_hs(temp2);
        }
    }

  // scan the polynomial terms, cumulatively computing the result:
  for (Size termNo = 0; termNo < psize; ++termNo)
    {
      //      printf("processing term %d...\n", termNo);
      Value termValue = eval_convert_hs(cf2val, terms[termNo].coeff);

      for (Var var = 0; var < maxArity; ++var)
        {
          Power pwr = terms[termNo].powers[var];
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

  // free pre-computed Chebyshev powers:
  for (Var var = 0; var < maxArity; ++var)
    {
      // free the haskell values pointed to by this array:
      for (Power power = 0; power < maxPowers[var]; ++power)
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

  return result;
}
