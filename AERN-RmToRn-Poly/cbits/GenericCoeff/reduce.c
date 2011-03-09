#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <tree234.h>

#include <GenericCoeff/coeff.h>
#include <GenericCoeff/poly.h>

/*
 * auxiliary function that copies a block of terms as is
 *
 * ASSUMES: deg <= maxDeg && size <= resMaxSize
 */
void
ADD_COEFF_CODE(copyTermsWithoutReduction)(Ops_Mutable * ops, Var arity,
    Poly * res, Term * resTerms, Size resPsize, Poly * src, Term * srcTerms,
    Size curPsize);

/*
 * a part of copyEnclUsingMutableOps that was split off into this function
 * (the case where some of the terms with lowest absolute value coeffs are removed
 *  and their number is SMALL compared to the number of terms that will be kept)
 */
void
ADD_COEFF_CODE(copyEnclUsingMutableOpsReduceSizeStoreCopiedTerms)(
    ComparisonOp compare, Ops_Mutable * ops, Var arity, Size curPsize,
    Size resPsize, Size resMaxSize, Term * srcTerms, Term * resTerms,
    CoeffMutable errorBound);

/*
 * a part of copyEnclUsingMutableOps that was split off into this function
 * (the case where some of the terms with lowest absolute value coeffs are removed
 *  and their number is LARGE compared to the number of terms that will be kept)
 */
void
ADD_COEFF_CODE(copyEnclUsingMutableOpsReduceSizeStoreReducedTerms)(
    ComparisonOp compare, Ops_Mutable * ops, Var arity, Size curPsize,
    Size resPsize, int reductionsNeeded, Term * srcTerms, Term * resTerms,
    CoeffMutable errorBound);

/*
 * ASSUMES: maxArity of src and res are the same
 * INVARIANT : does not change maxDeg and maxSize of res
 */
void
ADD_COEFF_CODE(copyEnclUsingMutableOps)(ComparisonOp compare,
    Ops_Mutable * ops, Poly * res, Poly * src)
{
  //  printf("copyEncl: entry\n");

  Var srcArity = src -> maxArity;
  Size srcPsize = src -> psize;
  Term * srcTerms = src -> terms;

  Size resPsize = res -> psize;
  Size resMaxSize = res -> maxSize;
  Power resMaxDeg = res -> maxDeg;
  Term * resTerms = res -> terms;

  CFM_ASSIGN(ops, res -> constTerm, src -> constTerm);

  Power curDeg = (srcPsize == 0) ? 0
      : MONOMIAL_DEGREE(srcTerms[srcPsize-1].powers);

  //  printf("copyEncl: srcDeg=%d resMaxDeg=%d srcPsize=%d resMaxSize=%d\n", curDeg, resMaxDeg, srcPsize, resMaxSize);

  if (curDeg <= resMaxDeg && srcPsize <= resMaxSize) // just copy src terms into res?
    {
      //    printf("copyEncl: no reduction needed\n");
      ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, srcArity, res, resTerms,
          resPsize, src, srcTerms, srcPsize);
      res -> psize = srcPsize;
      CFM_ASSIGN(ops, res -> errorBound, src -> errorBound);
    }
  else // some reduction will be needed
    {
      CoeffMutable errorBound = CFM_NEW(ops, CFM_SAMPLE(ops)); // error accumulator
      CFM_ASSIGN(ops, errorBound, src -> errorBound);

      CoeffMutable absError = CFM_NEW(ops, CFM_SAMPLE(ops)); // reduction error
      int curLastTermIndex = srcPsize - 1; // index of current last term in src
      //    printf("copyEncl: curLastTermIndex=%d\n", curLastTermIndex);
      while (curDeg > resMaxDeg) // degree reduce i.e. truncate last term?
        {
          CFM_ABS_UP(ops, absError, srcTerms[curLastTermIndex].coeff); // compute truncation error
          CFM_ADD_UP(ops, errorBound, errorBound, absError); // accumulate error
          curLastTermIndex--; // forget truncated term
          //      printf("copyEncl: degree reduced term\n");
          curDeg = (curLastTermIndex < 0) ? 0
              : MONOMIAL_DEGREE(srcTerms[curLastTermIndex].powers);
          //      printf("copyEncl: curDeg=%d curLastTermIndex=%d\n", curDeg, curLastTermIndex);
          //          printf(
          //              "copyEncl: curDeg=%d resMaxDeg=%d curPsize=%d resMaxSize=%d\n",
          //              curDeg, resMaxDeg, curLastTermIndex + 1, resMaxSize);
        }
      CFM_FREE(absError);
      Size curPsize = curLastTermIndex + 1; // truncate reduced terms
      //    printf("copyEncl: curDeg=%d resMaxDeg=%d curPsize=%d resMaxSize=%d\n", curDeg, resMaxDeg, curPsize, resMaxSize);

      int reductionsNeeded = curPsize - resMaxSize;
      //    printf("copyEncl: %d reductionsNeeded\n", reductionsNeeded);
      if (reductionsNeeded <= 0) // nothing to reduce i.e. copy remaining src terms into res?
        {
          ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, srcArity, res,
              resTerms, resPsize, src, srcTerms, curPsize);
        }
      else // curPsize > resMaxSize i.e. size reduction is needed
        {
          if (reductionsNeeded >= resMaxSize) // keep copied i.e. large terms in tree
            {
              ADD_COEFF_CODE(copyEnclUsingMutableOpsReduceSizeStoreCopiedTerms)(
                  compare, ops, srcArity, curPsize, resPsize, resMaxSize,
                  srcTerms, resTerms, errorBound);
            }
          else // keep reduced i.e. small terms in tree
            {
              ADD_COEFF_CODE(copyEnclUsingMutableOpsReduceSizeStoreReducedTerms)(
                  compare, ops, srcArity, curPsize, resPsize, reductionsNeeded,
                  srcTerms, resTerms, errorBound);
            }
          res -> psize = resMaxSize;
        }
      CFM_ASSIGN(ops, res -> errorBound, errorBound);
    }
}



void
ADD_COEFF_CODE(copyTermsWithoutReduction)(Ops_Mutable * ops, Var arity,
    Poly * res, Term * resTerms, Size resPsize, Poly * src, Term * srcTerms,
    Size curPsize)
{
  //  printf("copyTerms: entry\n");
  int i = 0; // save allocating and initialising one int by using two whiles
  while (i < curPsize) // for each term in current version of src
    {
      if (resPsize <= i) // ith coefficient in res points to null?
        {
          resTerms[i].coeff = CFM_NEW(ops, CFM_SAMPLE(ops)); // new it up
        }
      CFM_ASSIGN(ops, resTerms[i].coeff, srcTerms[i].coeff); // copy the coefficient
      //    printf("copyTerms: copied coeff\n");
      memmove(resTerms[i].powers, srcTerms[i].powers, SIZEOF_POWERS(arity)); // and powers
      //    printf("copyTerms: copied powers\n");
      i++;
      //      printf("copyTerms: copied term\n");
    }
  while (i < resPsize) // any terms left unassigned in res?
    {
      CFM_FREE(resTerms[i].coeff); // free their coefficients
      //      printf("copyTerms: freed coeff\n");
      i++;
    }
  res -> psize = curPsize; // forget remaining terms in res
  //  printf("copyTerms: exit\n");
}

typedef struct
{
  CoeffMutable coeff;
  int index;
  ComparisonOp compare;
} CoeffFor234;

int
compareFor234(CoeffFor234 * dp1, CoeffFor234 * dp2)
{
  int cfcmp = CF_COMPARE(dp1 -> compare, dp1 -> coeff, dp2 -> coeff);
  return (cfcmp) ? cfcmp : (dp1 -> index - dp2 -> index);
  printf("compareFor234: compared\n");
}

void
ADD_COEFF_CODE(copyEnclUsingMutableOpsReduceSizeStoreCopiedTerms)(
    ComparisonOp compare, Ops_Mutable * ops, Var arity, Size curPsize,
    Size resPsize, Size resMaxSize, Term * srcTerms, Term * resTerms,
    CoeffMutable errorBound)
{
  bool * reducedTerms = calloc(curPsize, sizeof(bool)); // set reduced terms to true, all initialised as false
  int termIndex = 0;

  tree234 * tree = newtree234(&compareFor234); // ordered storage
  //        printf("copyEncl: %d elements in tree\n", count234(tree));

  //        printf("\nreductionsNeeded >= resMaxSize\n\n");

  int fullTreeSize = resMaxSize;

  //        printf("copyEncl: fullTreeSize=%d\n", fullTreeSize);
  while (termIndex < fullTreeSize) // fill tree with fullTreeSize elements
    {
      CoeffFor234 * c = malloc(sizeof(CoeffFor234));
      c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
      CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
      c -> index = termIndex;
      c -> compare = compare;
      add234(tree, c);
      //          printf("copyEncl: added element\n");
      //          printf("copyEncl: %d elements in tree\n", count234(tree));
      termIndex++;
    }

  //        printf("copyEncl: tree full\n");

  CoeffFor234 * coeffP; // pointer to tree element
  while (termIndex < curPsize) // push an element and pop the smallest
    {
      CoeffFor234 * c = malloc(sizeof(CoeffFor234)); // allocate an element
      c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
      CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
      c -> index = termIndex;
      c -> compare = compare;
      add234(tree, c); // add the element
      //          printf("copyEncl: added element\n");
      //          printf("copyEncl: %d elements in tree\n", count234(tree));
      coeffP = delpos234(tree, 0); // pop the least element
      //          printf("copyEncl: found least element\n");
      CFM_ADD_UP(ops, errorBound, errorBound, coeffP -> coeff); // accumulate reduction error
      //          printf("copyEncl: accumulated reduction error\n");
      CFM_FREE(coeffP -> coeff);
      //          printf("copyEncl: freed coefficient\n");
      reducedTerms[coeffP -> index] = true; // mark the original position of reduced term in srcTerms
      free(coeffP); // delete and free the element
      //          printf("copyEncl: deleted least element\n");
      //          printf("copyEncl: %d elements in tree\n", count234(tree));
      termIndex++;
    }

  //        printf("\ncopyEncl: reduction complete\n\n");

  for (int i = 0; i < fullTreeSize; i++) // free tree and contents of elements
    {
      coeffP = delpos234(tree, 0); // delete reference to struct from tree
      //          printf("copyEncl: deleted element number %d\n", i);
      CFM_FREE(coeffP -> coeff);
      //          printf("copyEncl: freed coefficient\n");
      free(coeffP); // free the struct
      //          printf("copyEncl: freed element\n");
      //          printf("copyEncl: %d elements in tree\n", count234(tree));
    }
  freetree234(tree); // free the tree

  //        printf("\ncopyEncl: tree freed\n\n");

  //    printf("copyEncl: curPsize=%d\n", curPsize);
  int srcIndex = 0;
  int resIndex = 0;
  //    printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
  while (srcIndex < curPsize)
    {
      if (reducedTerms[srcIndex] == false) // term not reduced i.e. copy term?
        {
          if (resPsize <= resIndex) // coefficient to be written in res not allocated?
            {
              resTerms[resIndex].coeff = CFM_NEW(ops, CFM_SAMPLE(ops)); // new it up
            }
          CFM_ASSIGN(ops, resTerms[resIndex].coeff, srcTerms[srcIndex].coeff); // copy the coefficient
          //    printf("copyTerms: copied coeff\n");
          memmove(resTerms[resIndex].powers, srcTerms[srcIndex].powers,
              SIZEOF_POWERS(arity)); // and powers
          //    printf("copyTerms: copied powers\n");
          //    printf("copyEncl: copied term\n");
          resIndex++;
        }
      srcIndex++;
      //    printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
    }
  free(reducedTerms);
}

void
ADD_COEFF_CODE(copyEnclUsingMutableOpsReduceSizeStoreReducedTerms)(
    ComparisonOp compare, Ops_Mutable * ops, Var arity, Size curPsize,
    Size resPsize, int reductionsNeeded, Term * srcTerms, Term * resTerms,
    CoeffMutable errorBound)
{
  bool * reducedTerms = calloc(curPsize, sizeof(bool)); // set reduced terms to true, all initialised as false
  int termIndex = 0;

  tree234 * tree = newtree234(&compareFor234); // ordered storage
  //        printf("copyEncl: %d elements in tree\n", count234(tree));

  //        printf("\nreductionsNeeded < resMaxSize\n\n");

  int fullTreeSize = reductionsNeeded;

  //        printf("copyEncl: fullTreeSize=%d\n", fullTreeSize);
  while (termIndex < fullTreeSize) // fill tree with treeSize elements
    {
      CoeffFor234 * c = malloc(sizeof(CoeffFor234));
      c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
      CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
      c -> index = termIndex;
      c -> compare = compare;
      add234(tree, c);
      //          printf("copyEncl: added element\n");
      //          printf("copyEncl: %d elements in tree\n", count234(tree));
      termIndex++;
    }

  //        printf("\ncopyEncl: tree full\n\n");

  CoeffFor234 * coeffP; // pointer to tree element

  while (termIndex < curPsize) // push an element and pop the largest
    {
      CoeffFor234 * c = malloc(sizeof(CoeffFor234)); // allocate an element
      c -> coeff = CFM_NEW(ops, CFM_SAMPLE(ops));
      CFM_ABS_UP(ops, c -> coeff, srcTerms[termIndex].coeff);
      c -> index = termIndex;
      c -> compare = compare;
      add234(tree, c); // add the element
      //          printf("copyEncl: added element\n");
      //          printf("copyEncl: %d elements in tree\n", count234(tree));
      coeffP = delpos234(tree, fullTreeSize); // delete reference to element
      //          printf("copyEncl: deleted greatest element\n");
      CFM_FREE(coeffP -> coeff);
      //          printf("copyEncl: freed the coefficient\n");
      free(coeffP);
      //          printf("copyEncl: freed the element\n");
      //          printf("copyEncl: %d elements in tree\n", count234(tree));
      termIndex++;
    }

  //        printf("\ncopyEncl: reduction complete\n\n");

  for (int i = 0; i < fullTreeSize; i++) // free tree and contents of elements
    {
      coeffP = delpos234(tree, 0);
      //          printf("copyEncl: deleted element\n");
      CFM_ADD_UP(ops, errorBound, errorBound, coeffP -> coeff); // accumulate reduction error
      //          printf("copyEncl: accumulated reduction error\n");
      reducedTerms[coeffP -> index] = true; // mark the original position of reduced term in srcTerms
      CFM_FREE(coeffP -> coeff);
      //          printf("copyEncl: freed the coefficient\n");
      free(coeffP); // free the element
      //          printf("copyEncl: freed the element\n");
      //          printf("copyEncl: %d elements left in tree\n", count234(tree));
    }
  freetree234(tree); // free the tree

  //        printf("\ncopyEncl: tree freed\n\n");

  //        printf("copyEncl: curPsize=%d\n", curPsize);
  int srcIndex = 0;
  int resIndex = 0;
  //        printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
  while (srcIndex < curPsize)
    {
      if (reducedTerms[srcIndex] == false) // term not reduced i.e. copy term?
        {
          if (resPsize <= resIndex) // coefficient to be written in res not allocated?
            {
              resTerms[resIndex].coeff = CFM_NEW(ops, CFM_SAMPLE(ops)); // new it up
            }
          CFM_ASSIGN(ops, resTerms[resIndex].coeff, srcTerms[srcIndex].coeff); // copy the coefficient
          //    printf("copyTerms: copied coeff\n");
          memmove(resTerms[resIndex].powers, srcTerms[srcIndex].powers,
              SIZEOF_POWERS(arity)); // and powers
          //    printf("copyTerms: copied powers\n");
          //            printf("copyEncl: copied term\n");
          resIndex++;
        }
      srcIndex++;
      //          printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
    }
  free(reducedTerms);
}

//void
//ADD_COEFF_CODE(reduceDegreeEnclUsingMutableOps)(Ops_Mutable * ops,
//    Power maxDeg, Poly * p)
//{
//  Term * terms = p -> terms;
//  Size oldSize = p -> psize;
//  Size lastTermIndex = oldSize - 1; // track last term for swapping
//  CoeffMutable maxError = CFM_NEW(ops, CFM_SAMPLE(ops));
//  CoeffMutable errorBound = CFM_NEW(ops, CFM_SAMPLE(ops));
//  for (int i = 0; i <= lastTermIndex; i++)
//    {
//      if (MONOMIAL_DEGREE(terms[i].powers) > maxDeg && lastTermIndex > 0) // reduce term?
//        {
//          CFM_ABS_UP(ops, maxError, terms[i].coeff); // compute reduction error
//          CFM_FREE(terms[i].coeff); // free ith coefficient
//          CFM_ADD_UP(ops, errorBound, errorBound, maxError); // accumulate rounding error
//          // shift terms cheaply by swapping the ith and the last term
//          terms[i].powers = terms[lastTermIndex].powers; // point ith powers to last
//          terms[i].coeff = terms[lastTermIndex].coeff; // point ith coefficient to last
//          lastTermIndex--; // forget last term, i.e. decrement psize (implicitly)
//        }
//    }
//  if (MONOMIAL_DEGREE(terms[lastTermIndex].powers) > maxDeg) // reduce last term?
//    {
//      // QUESTION: cheaper to use a local Coeff coeffLast = terms[lastTermIndex].coeff ?
//
//      CFM_ABS_UP(ops, maxError, terms[lastTermIndex].coeff); // compute reduction error
//      CFM_FREE(terms[lastTermIndex].coeff); // free last coefficient
//      CFM_ADD_UP(ops, errorBound, errorBound, maxError); // accumulate rounding error
//
//      lastTermIndex--; // forget last term, i.e. decrement psize (implicitly)
//    }
//  CFM_FREE(maxError);
//  CFM_ASSIGN(ops, p -> errorBound, errorBound); // account for rounding errors
//  CFM_FREE(errorBound);
//
//  lastTermIndex++; // set lastTermIndex to psize
//  if (oldSize > lastTermIndex) // any terms reduced?
//    {
//      p -> psize = lastTermIndex; // update psize
//    }
//}
