#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP cdclogis(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cdcnorm(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cdtlogis(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cdtnorm(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cpclogis(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cpcnorm(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cptlogis(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cptnorm(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"cdclogis", (DL_FUNC) &cdclogis, 6},
    {"cdcnorm",  (DL_FUNC) &cdcnorm,  6},
    {"cdtlogis", (DL_FUNC) &cdtlogis, 6},
    {"cdtnorm",  (DL_FUNC) &cdtnorm,  6},
    {"cpclogis", (DL_FUNC) &cpclogis, 7},
    {"cpcnorm",  (DL_FUNC) &cpcnorm,  7},
    {"cptlogis", (DL_FUNC) &cptlogis, 7},
    {"cptnorm",  (DL_FUNC) &cptnorm,  7},
    {NULL, NULL, 0}
};

void R_init_foehnix(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

