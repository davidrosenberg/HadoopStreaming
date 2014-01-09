#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>

// copied from unique.c -- not sure how to just load it...
static int sequal(SEXP x, int i, SEXP y, int j)
{
  SEXP a = STRING_ELT(x, i);
  SEXP b = STRING_ELT(y, j);
  if (i < 0 || j < 0) return 0;
  /* Two strings which have the same address must be the same,
     so avoid looking at the contents */
  if (a == b ) {
    //    printf("Same pointer value.\n");
    return 1;
  }
  /* Then if either is NA the other cannot be */
  /* Once all CHARSXPs are cached, Seql will handle this */
  if (a == NA_STRING || b == NA_STRING) {
    //    printf("at least one is NA_STRING\n");
    return 0;
  }
  if (LENGTH(a) != LENGTH(b)) {
    //    printf("Different lengths\n");
    return 0;
  }
  //  printf("directly comparing strings\n");
  return !strcmp(CHAR(a),CHAR(b));
}



SEXP nonMatch(SEXP Rval,SEXP Rvec ) {
  int n = length(Rvec);
  int i;  // stores location of nonmatch
  if (isNumeric(Rvec)) {
    double *vec = REAL(Rvec);
    double value=REAL(Rval)[0];
    for (i=0; i<n; ++i) {
      if (vec[i] != value) break;
    }
  } 
  else if (isString(Rvec))  {
    const char *value =CHAR(STRING_ELT(Rval,0));
    for (i=0; i<n; ++i) {
      //if (strcmp(CHAR(STRING_ELT(Rvec,i) ),value)!=0) 	break;
      if (!sequal(Rvec,i,Rval,0)) 	break;
    }
  }
  else {
    printf("NOT NUMERIC OR CHARACTER!!\n");
    return R_NilValue;
  }
  if (i==n) {
    return R_NilValue;
  }
  PROTECT(Rval = allocVector(INTSXP,1));
  INTEGER(Rval)[0] = i+1;
  UNPROTECT(1);
  return Rval;
}
    
    
//library(HadoopStreaming,lib.loc="~/RLibrary")
//a=c(rep('111111',20000000),rep('222222',1));
//b=proc.time();nonMatch(a[1],a);proc.time()-b
