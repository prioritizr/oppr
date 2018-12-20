#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _ppr_rcpp_branch_matrix(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_ppr_rcpp_branch_matrix", (DL_FUNC) &_ppr_rcpp_branch_matrix, 1},
    {NULL, NULL, 0}
};

void R_init_ppr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
