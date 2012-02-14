#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <tree234.h>

#include <GenericCoeff/coeff.h>
#include <GenericCoeff/poly.h>

//#define DEBUG_REDUCE(x) x;
#define DEBUG_REDUCE(x)

/*
 * auxiliary function that copies a block of terms as is
 *
 * ASSUMES: deg <= maxDeg && size <= resMaxSize
 */
void
ADD_COEFF_CODE(copyTermsWithoutReduction)(Ops * ops, Var arity, Poly * res,
    Term * resTerms, Size resPsize, Poly * src, Term * srcTerms, Size curPsize);

/*
 * auxiliary function that copies a subset of a block of terms
 *
 * ASSUMES: deg <= maxDeg && size <= resMaxSize
 */
void
ADD_COEFF_CODE(copySomeTermsWithoutReduction)(Ops * ops, Var arity, Poly * res,
    Term * resTerms, Size resPsize, Poly * src, Term * srcTerms, Size curPsize,
    bool * reducedTerms);

/*
 * a part of copyEncl that was split off into this function
 * (the case where some of the terms with lowest absolute value coeffs are removed
 *  and their number is SMALL compared to the number of terms that will be kept)
 */
void
ADD_COEFF_CODE(copyEnclReduceSizeStoreCopiedTerms)(Ops * ops, Var arity,
    Size curPsize, bool * reducedTerms, Size resPsize, Size resMaxSize,
    Term * srcTerms, Term * resTerms, CoeffMutable * errorBoundPtr);

/*
 * a part of copyEncl that was split off into this function
 * (the case where some of the terms with lowest absolute value coeffs are removed
 *  and their number is LARGE compared to the number of terms that will be kept)
 */
void
ADD_COEFF_CODE(copyEnclReduceSizeStoreReducedTerms)(Ops * ops, Var arity,
    Size curPsize, bool * reducedTerms, Size resPsize, int reductionsNeeded,
    Term * srcTerms, Term * resTerms, CoeffMutable * errorBoundPtr);

/*
 * ASSUMES: maxArity, maxSize, maxDeg as well as maxTermArity of src and res are the same
 */
void
ADD_COEFF_CODE(copySameSizes)(Ops * ops, Poly * res, Poly * src)
{
  //  printf("copySameSizes: entry\n");

  Var srcArity = src -> maxArity;
  Size srcPsize = src -> psize;
  Term * srcTerms = src -> terms;

  Size resPsize = res -> psize;
  Term * resTerms = res -> terms;

  CFM_ASSIGN(ops, res -> constTerm, src -> constTerm);

  ADD_COEFF_CODE(copyTermsWithoutReduction)(ops, srcArity, res, resTerms,
      resPsize, src, srcTerms, srcPsize);

  res -> psize = srcPsize;

  CFM_ASSIGN(ops, res -> errorBound, src -> errorBound);
}

/*
 * ASSUMES: maxArity of src and res are the same
 * INVARIANT : does not change maxDeg, maxSize and maxTermArity of res
 */
void
ADD_COEFF_CODE(copyEncl)(Ops * ops, Poly * res, Poly * src)
{
  DEBUG_REDUCE(printf("copyEncl: entry\n"));

  Var srcArity = src -> maxArity;
  Size srcPsize = src -> psize;
  Size srcMaxTermArity = src -> maxTermArity;
  Term * srcTerms = src -> terms;

  Size resPsize = res -> psize;
  Size resMaxSize = res -> maxSize;
  Power resMaxDeg = res -> maxDeg;
  Power resMaxTermArity = res -> maxTermArity;
  Term * resTerms = res -> terms;

  // this is always needed:
  CFM_ASSIGN(ops, res -> constTerm, src -> constTerm);

  // initialise an error accumulator:
  CoeffMutable errorBound = res -> errorBound;
  CFM_ASSIGN(ops, errorBound, src -> errorBound);

  // set up a temp variable:
  CoeffMutable temp = CFM_NEW(ops, CFM_ZERO(ops));

  // first, deal with terms that exceed the maximum arity
  // if there is any risk of that happening:
  bool * reducedTerms = calloc(srcPsize, sizeof(bool)); // all false to start with
  int termArityExceededCount = 0;

  if (resMaxTermArity < srcMaxTermArity)
    {
      for (int i = 0; i < srcPsize; ++i)
        {
          int exceeded = TERM_ARITY(srcTerms[i].powers) > resMaxTermArity;
          if (exceeded)
            {
              reducedTerms[i] = true;
              termArityExceededCount++; // increment counter to help detect presence of this
              // this term will be ignored - compensate for this in the errorBound:
              CFM_ABS_UP(ops, temp, srcTerms[i].coeff);
              CFM_ADD_UP(ops, errorBound, errorBound, temp);
              // value of temp no longer needed
            }
          // do not check terms whose degree is too high anyway:
          if (MONOMIAL_DEGREE(srcTerms[i].powers) > resMaxDeg)
            {
              break;
            }
        }
    }

  // now deal with terms that exceed degree limit - all at the end due to the term ordering

  // the last term has the highest degree:
  Power curDeg = (srcPsize == 0) ? 0
      : MONOMIAL_DEGREE(srcTerms[srcPsize-1].powers);

  //  printf("copyEncl: srcDeg=%d resMaxDeg=%d srcPsize=%d resMaxSize=%d\n", curDeg, resMaxDeg, srcPsize, resMaxSize);

  int curLastTermIndex = srcPsize - 1; // index of current last term in src
  //    printf("copyEncl: curLastTermIndex=%d\n", curLastTermIndex);
  while (curDeg > resMaxDeg) // degree reduce i.e. truncate last term?

    {
      CoeffMutable absError = temp;
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
  Size curPsize = curLastTermIndex + 1; // truncate reduced terms
  //    printf("copyEncl: curDeg=%d resMaxDeg=%d curPsize=%d resMaxSize=%d\n", curDeg, resMaxDeg, curPsize, resMaxSize);

  int reductionsNeeded = curPsize - termArityExceededCount - resMaxSize;
  //    printf("copyEncl: %d reductionsNeeded\n", reductionsNeeded);
  if (reductionsNeeded <= 0) // nothing to reduce except terms that exceed arity limit
    {
      // copy remaining legal src terms into res:
      ADD_COEFF_CODE(copySomeTermsWithoutReduction)(ops, srcArity, res,
          resTerms, resPsize, src, srcTerms, curPsize, reducedTerms);
    }
  else // curPsize - termArityExceededCount > resMaxSize i.e. size reduction is needed

    {
      if (reductionsNeeded >= resMaxSize) // keep copied i.e. large terms in tree

        {
          ADD_COEFF_CODE(copyEnclReduceSizeStoreCopiedTerms)(ops, srcArity,
              curPsize, reducedTerms, resPsize, resMaxSize, srcTerms, resTerms,
              &errorBound);
        }
      else // keep reduced i.e. small terms in tree

        {
          ADD_COEFF_CODE(copyEnclReduceSizeStoreReducedTerms)(ops, srcArity,
              curPsize, reducedTerms, resPsize, reductionsNeeded, srcTerms,
              resTerms, &errorBound);
        }
      res -> psize = resMaxSize;
    }

  CFM_ASSIGN(ops, res -> errorBound, errorBound);

  CFM_FREE(temp);
  FREE(reducedTerms);

  DEBUG_REDUCE(printf("copyEncl: exit\n"));
  DEBUG_REDUCE(printf("copyEncl: res = \n"));
  DEBUG_REDUCE(ADD_COEFF_CODE(printPoly)(res));
}

/*
 * quick and dirty version using copyEncl.. to be revisited to save errorBound and constCoeff copying
 */
void
ADD_COEFF_CODE(copyUpThin)(Ops * ops, Poly * res, Poly * src)
{
  ADD_COEFF_CODE(copyEncl)(ops, res, src); // copy src into res
  CFM_ADD_UP(ops, res -> constTerm, res -> constTerm, res -> errorBound); // account for errorBound
  CFM_ASSIGN_VAL(ops, res -> errorBound, CFM_ZERO(ops)); // collapse errorBound
}

/*
 * quick and dirty version using copyEncl.. to be revisited to save errorBound and constCoeff copying
 */
void
ADD_COEFF_CODE(copyDnThin)(Ops * ops, Poly * res, Poly * src)
{
  ADD_COEFF_CODE(copyEncl)(ops, res, src); // copy src into res
  CFM_SUB_DN(ops, res -> constTerm, res -> constTerm, res -> errorBound); // account for errorBound
  CFM_ASSIGN_VAL(ops, res -> errorBound, CFM_ZERO(ops)); // collapse errorBound
}

void
ADD_COEFF_CODE(copyTermsWithoutReduction)(Ops * ops, Var arity, Poly * res,
    Term * resTerms, Size resPsize, Poly * src, Term * srcTerms, Size curPsize)
{
  //  printf("copyTerms: entry\n");
  int i = 0; // index of term in src and of corresponding term in res
  while (i < curPsize) // for each term in current version of src
    {
      if (resPsize <= i) // ith coefficient in res points to null?
        {
          resTerms[i].coeff = CFM_NEW(ops, CFM_ZERO(ops)); // new it up
        }
      CFM_ASSIGN(ops, resTerms[i].coeff, srcTerms[i].coeff); // copy the coefficient
      //    printf("copyTerms: copied coeff\n");
      memmove(resTerms[i].powers, srcTerms[i].powers,
          SIZEOF_POWERS(arity)); // and powers
      //    printf("copyTerms: copied powers\n");
      i++;
      //      printf("copyTerms: copied term\n");
    }
  res -> psize = curPsize; // forget remaining terms in res
  while (i < resPsize) // any terms left unassigned in res?
    {
      CFM_FREE(resTerms[i].coeff); // free their coefficients
      //      printf("copyTerms: freed coeff\n");
      i++;
    }
  //  printf("copyTerms: exit\n");
}

void
ADD_COEFF_CODE(copySomeTermsWithoutReduction)(Ops * ops, Var arity, Poly * res,
    Term * resTerms, Size resPsize, Poly * src, Term * srcTerms, Size curPsize,
    bool * reducedTerms)
{
  //  printf("copyTerms: entry\n");
  int iSrc = 0; // index of term in src
  int iRes = 0; // index of corresponding term in res
  while (iSrc < curPsize) // for each term in current version of src
    {
      if (!reducedTerms[iSrc])
        {
          if (resPsize <= iRes) // ith coefficient in res points to null?
            {
              resTerms[iRes].coeff = CFM_NEW(ops, CFM_ZERO(ops)); // new it up
            }
          CFM_ASSIGN(ops, resTerms[iRes].coeff, srcTerms[iSrc].coeff); // copy the coefficient
          //    printf("copyTerms: copied coeff\n");
          memmove(resTerms[iRes].powers, srcTerms[iSrc].powers,
              SIZEOF_POWERS(arity)); // and powers
          //    printf("copyTerms: copied powers\n");
          iRes++;
        }
      iSrc++;
      //      printf("copyTerms: copied term\n");
    }
  res -> psize = iRes; // forget remaining terms in res
  while (iRes < resPsize) // any terms left unassigned in res?
    {
      CFM_FREE(resTerms[iRes].coeff); // free their coefficients
      //      printf("copyTerms: freed coeff\n");
      iRes++;
    }
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
  int cfcmp = CFM_COMPARE(dp1 -> compare, dp1 -> coeff, dp2 -> coeff);
  return (cfcmp) ? cfcmp : ((dp1 -> index) - (dp2 -> index));
  //  printf("compareFor234: compared\n");
}

void
ADD_COEFF_CODE(copyEnclReduceSizeStoreCopiedTerms)(Ops * ops, Var arity,
    Size curPsize, bool * reducedTerms, Size resPsize, Size resMaxSize,
    Term * srcTerms, Term * resTerms, CoeffMutable * errorBoundPtr)
{

  tree234 * tree = newtree234((cmpfn234) &compareFor234); // ordered storage
  //        printf("copyEncl: %d elements in tree\n", count234(tree));

  //        printf("\nreductionsNeeded >= resMaxSize\n\n");


  //        printf("copyEncl: fullTreeSize=%d\n", fullTreeSize);
  int fullTreeSize = resMaxSize;

  int termIndex = 0;
  int iSrc = 0;
  while (termIndex < fullTreeSize) // fill tree with fullTreeSize elements
    {
      if (!reducedTerms[iSrc])
        {
          CoeffFor234 * c = malloc(sizeof(CoeffFor234));
          c -> coeff = CFM_NEW(ops, CFM_ZERO(ops));
          CFM_ABS_UP(ops, c -> coeff, srcTerms[iSrc].coeff);
          c -> index = iSrc;
          c -> compare = CFM_COMPARE_FN(ops);
          add234(tree, c);
          //          printf("copyEncl: added element\n");
          //          printf("copyEncl: %d elements in tree\n", count234(tree));
          termIndex++;
        }
      iSrc++;
    }

  //        printf("copyEncl: tree full\n");

  CoeffFor234 * coeffP; // pointer to tree element
  while (iSrc < curPsize) // push an element and pop the smallest
    {
      if (!reducedTerms[iSrc])
        {
          CoeffFor234 * c = malloc(sizeof(CoeffFor234)); // allocate an element
          c -> coeff = CFM_NEW(ops, CFM_ZERO(ops));
          CFM_ABS_UP(ops, c -> coeff, srcTerms[iSrc].coeff);
          c -> index = iSrc;
          c -> compare = CFM_COMPARE_FN(ops);
          add234(tree, c); // add the element
          //          printf("copyEncl: added element\n");
          //          printf("copyEncl: %d elements in tree\n", count234(tree));
          coeffP = delpos234(tree, 0); // pop the least element
          //          printf("copyEncl: found least element\n");
          CFM_ADD_UP(ops, * errorBoundPtr, * errorBoundPtr, coeffP -> coeff); // accumulate reduction error
          //          printf("copyEncl: accumulated reduction error\n");
          CFM_FREE(coeffP -> coeff);
          //          printf("copyEncl: freed coefficient\n");
          reducedTerms[coeffP -> index] = true; // mark the original position of reduced term in srcTerms
          FREE(coeffP); // delete and free the element
          //          printf("copyEncl: deleted least element\n");
          //          printf("copyEncl: %d elements in tree\n", count234(tree));
        }
      iSrc++;
    }

  //        printf("\ncopyEncl: reduction complete\n\n");

  for (int i = 0; i < fullTreeSize; i++) // free tree and contents of elements

    {
      coeffP = delpos234(tree, 0); // delete reference to struct from tree
      //          printf("copyEncl: deleted element number %d\n", i);
      CFM_FREE(coeffP -> coeff);
      //          printf("copyEncl: freed coefficient\n");
      FREE(coeffP); // free the struct
      //          printf("copyEncl: freed element\n");
      //          printf("copyEncl: %d elements in tree\n", count234(tree));
    }
  freetree234(tree); // free the tree

  //        printf("\ncopyEncl: tree freed\n\n");

  //    printf("copyEncl: curPsize=%d\n", curPsize);
  iSrc = 0;
  int iRes = 0;
  //    printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
  while (iSrc < curPsize)
    {
      if (!reducedTerms[iSrc]) // term not reduced i.e. copy term?

        {
          if (resPsize <= iRes) // coefficient to be written in res not allocated?

            {
              resTerms[iRes].coeff = CFM_NEW(ops, CFM_ZERO(ops)); // new it up
            }
          CFM_ASSIGN(ops, resTerms[iRes].coeff, srcTerms[iSrc].coeff); // copy the coefficient
          //    printf("copyTerms: copied coeff\n");
          memmove(resTerms[iRes].powers, srcTerms[iSrc].powers,
              SIZEOF_POWERS(arity)); // and powers
          //    printf("copyTerms: copied powers\n");
          //    printf("copyEncl: copied term\n");
          iRes++;
        }
      iSrc++;
      //    printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
    }
}

void
ADD_COEFF_CODE(copyEnclReduceSizeStoreReducedTerms)(Ops * ops, Var arity,
    Size curPsize, bool * reducedTerms, Size resPsize, int reductionsNeeded,
    Term * srcTerms, Term * resTerms, CoeffMutable * errorBoundPtr)
{

  tree234 * tree = newtree234((cmpfn234) &compareFor234); // ordered storage
  //        printf("copyEncl: %d elements in tree\n", count234(tree));

  //        printf("\nreductionsNeeded < resMaxSize\n\n");

  int fullTreeSize = reductionsNeeded;

  int termIndex = 0;
  int iSrc = 0;

  //        printf("copyEncl: fullTreeSize=%d\n", fullTreeSize);
  while (termIndex < fullTreeSize) // fill tree with treeSize elements
    {
      if (!reducedTerms[iSrc])
        {
          CoeffFor234 * c = malloc(sizeof(CoeffFor234));
          c -> coeff = CFM_NEW(ops, CFM_ZERO(ops));
          CFM_ABS_UP(ops, c -> coeff, srcTerms[iSrc].coeff);
          c -> index = iSrc;
          c -> compare = CFM_COMPARE_FN(ops);
          add234(tree, c);
          //          printf("copyEncl: added element\n");
          //          printf("copyEncl: %d elements in tree\n", count234(tree));
          termIndex++;
        }
      iSrc++;
    }

  //        printf("\ncopyEncl: tree full\n\n");

  CoeffFor234 * coeffP; // pointer to tree element

  while (iSrc < curPsize) // push an element and pop the largest
    {
      CoeffFor234 * c = malloc(sizeof(CoeffFor234)); // allocate an element
      c -> coeff = CFM_NEW(ops, CFM_ZERO(ops));
      CFM_ABS_UP(ops, c -> coeff, srcTerms[iSrc].coeff);
      c -> index = iSrc;
      c -> compare = CFM_COMPARE_FN(ops);
      add234(tree, c); // add the element
      //          printf("copyEncl: added element\n");
      //          printf("copyEncl: %d elements in tree\n", count234(tree));
      coeffP = delpos234(tree, fullTreeSize); // delete reference to element
      //          printf("copyEncl: deleted greatest element\n");
      CFM_FREE(coeffP -> coeff);
      //          printf("copyEncl: freed the coefficient\n");
      FREE(coeffP);
      //          printf("copyEncl: freed the element\n");
      //          printf("copyEncl: %d elements in tree\n", count234(tree));
      iSrc++;
    }

  //        printf("\ncopyEncl: reduction complete\n\n");

  for (int i = 0; i < fullTreeSize; i++) // free tree and contents of elements
    {
      coeffP = delpos234(tree, 0);
      //          printf("copyEncl: deleted element\n");
      CFM_ADD_UP(ops, * errorBoundPtr, * errorBoundPtr, coeffP -> coeff); // accumulate reduction error
      //          printf("copyEncl: accumulated reduction error\n");
      reducedTerms[coeffP -> index] = true; // mark the original position of reduced term in srcTerms
      CFM_FREE(coeffP -> coeff);
      //          printf("copyEncl: freed the coefficient\n");
      FREE(coeffP); // free the element
      //          printf("copyEncl: freed the element\n");
      //          printf("copyEncl: %d elements left in tree\n", count234(tree));
    }
  freetree234(tree); // free the tree

  //        printf("\ncopyEncl: tree freed\n\n");

  //        printf("copyEncl: curPsize=%d\n", curPsize);
  iSrc = 0;
  int iRes = 0;
  //        printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
  while (iSrc < curPsize)
    {
      if (!reducedTerms[iSrc]) // term not reduced i.e. copy term?
        {
          if (resPsize <= iRes) // coefficient to be written in res not allocated?
            {
              resTerms[iRes].coeff = CFM_NEW(ops, CFM_ZERO(ops)); // new it up
            }
          CFM_ASSIGN(ops, resTerms[iRes].coeff, srcTerms[iSrc].coeff); // copy the coefficient
          //    printf("copyTerms: copied coeff\n");
          memmove(resTerms[iRes].powers, srcTerms[iSrc].powers,
              SIZEOF_POWERS(arity)); // and powers
          //    printf("copyTerms: copied powers\n");
          //            printf("copyEncl: copied term\n");
          iRes++;
        }
      iSrc++;
      //          printf("copyEncl: srcIndex=%d resIndex=%d\n", srcIndex, resIndex);
    }
}

//void
//ADD_COEFF_CODE(reduceDegreeEncl)(Ops * ops,
//    Power maxDeg, Poly * p)
//{
//  Term * terms = p -> terms;
//  Size oldSize = p -> psize;
//  Size lastTermIndex = oldSize - 1; // track last term for swapping
//  CoeffMutable maxError = CFM_NEW(ops, CFM_ZERO(ops));
//  CoeffMutable errorBound = CFM_NEW(ops, CFM_ZERO(ops));
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
