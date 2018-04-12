#include <R.h>
#include <Rmath.h>
#include <stdio.h>
#include <math.h>
void C_pmvhyper(int *x, int *nL, int *L, int *n, double *p, int *lower, int *logp);
void C_dmvhyper_logVal(int *x, int *nL, int *L, int *n, double *p, int *logp, double *logVal); //used in C_pmvhyper
void C_dmvhyper(int *x, int *nL, int *L, int *n, double *p, int *logp); //used in R function dpsets
int max2(int a, int b);
int min2(int a, int b);
int max(int a[], int n);
int min(int a[], int n);
double C_logChoose_logVal(int n, int k, double *logVal);
double C_logChoose(int n, int k);
double C_dhyper(int x, int w, int b, int n, int logp);
double C_dhyper_logVal(int x, int w, int b, int n, int logp, double *logVal);
void C_pdf_ABdiffC(int *x, int *L, int *n, double *p, int *logp); //not tested
void C_pdf_ABdiffC_logVal(int *x, int *L, int *n, double *p, int *logp, double *logVal); //not tested
void C_ABdiffC(int *x, int *L, int *n, double *p, int *lower, int *logp); //not tested
void C_AdiffBC(int *x, int *L, int *n, double *p, int *lower, int *logp); //not tested
void C_pdf_AdiffBC(int *x, int *L, int *n, double *p, int *logp); //not tested
#ifndef db_xmin
#define db_xmin (2.225074e-308) //machine minimum by R
#endif
