// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


/* Forward declarations */
void C_dmvhyper(int *x, int *nL, int *L, int *n, double *p, int *logp, int *errorCode);
void C_pmvhyper(int *x, int *nL, int *L, int *n, double *p, int *lower_tail, int *logp, int *errorCode);

static const R_CMethodDef CEntries[] = {
    {"C_dmvhyper", (DL_FUNC) &C_dmvhyper, 7},
    {"C_pmvhyper", (DL_FUNC) &C_pmvhyper, 8},
    {NULL, NULL, 0}
};

void R_init_SuperExactTest(DllInfo *dll) {
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
