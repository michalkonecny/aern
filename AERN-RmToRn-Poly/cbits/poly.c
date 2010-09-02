#include <stdio.h>
#include <stdlib.h>

#include "poly.h"
#include "EvalExport_stub.h"

Value
evalAtPtChebBasis(Poly * p, Value * values, Value one, BinaryOp add,
    BinaryOp subtr, BinaryOp mult, ConversionOp cf2val)
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
          varPowers[var][n] = eval_binary_hs(subtr, temp2, varPowers[var][n - 2]);
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
          if(pwr > 0)
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
      for(Power power = 0; power < maxPowers[var]; ++power)
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

Poly *
newConstPoly(Coeff c, Var maxArity, Size maxSize)
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
newProjectionPoly(Ops_Pure * ops, Var var, Var maxArity, Size maxSize)
{
  Poly * poly = newConstPoly(ops -> zero, maxArity, maxSize);

  // add one term for the variable:
  poly -> psize = 1;
  Term * term = poly -> terms;

  // initialise the term's coeff:
  term -> coeff = ops -> one;
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
