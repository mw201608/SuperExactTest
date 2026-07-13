/*
 * mvhyper.h
 * Optimized header for SuperExactTest multi-variate hypergeometric computations.
 *
 * Key improvements over the original:
 *   1. All intermediate sums are accumulated in LOG space (log-sum-exp trick)
 *      to avoid catastrophic cancellation and underflow when n >> L[i].
 *   2. C_logChoose uses lgamma() from <math.h> – a single call, no manual
 *      recursion, and it handles arbitrarily large arguments without overflow.
 *   3. Helper macros min/max/min2/max2 are guarded against double evaluation.
 *   4. db_xmin is exposed as the smallest representable positive double so
 *      callers can cap near-zero probabilities gracefully.
 *   5. No static recursion → no C-stack overflow for large n (fixes GitHub #1).
 */

#ifndef MVHYPER_H
#define MVHYPER_H

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <stdio.h>

/* Smallest representable positive double (avoids log(0)) */
#define db_xmin DBL_MIN
#define tiny -743.7469

/* log(0) sentinel used in log-sum-exp accumulations */
#define LOG_ZERO (-1.0/0.0)   /* -Inf */

/* Safe min/max macros – evaluate each argument only once */
static inline int _imax2(int a, int b) { return a > b ? a : b; }
static inline int _imin2(int a, int b) { return a < b ? a : b; }

#define max2(a,b) _imax2((a),(b))
#define min2(a,b) _imin2((a),(b))

/* max / min over an integer array of length n */
static inline int max(const int *arr, int n) {
    int m = arr[0];
    for (int i = 1; i < n; i++) if (arr[i] > m) m = arr[i];
    return m;
}
static inline int min(const int *arr, int n) {
    int m = arr[0];
    for (int i = 1; i < n; i++) if (arr[i] < m) m = arr[i];
    return m;
}

/* ------------------------------------------------------------------
 * log C(n, k)  –  uses lgamma for large-argument safety.
 * Returns -Inf when k < 0 or k > n (zero probability case).
 * ------------------------------------------------------------------ */
static inline double C_logChoose(int n, int k) {
    if (k < 0 || k > n) return LOG_ZERO;
    if (k == 0 || k == n) return 0.0;
    return lgamma((double)(n + 1))
         - lgamma((double)(k + 1))
         - lgamma((double)(n - k + 1));
}

/* ------------------------------------------------------------------
 * Log hypergeometric probability:
 *   log P(X = x | w white, b black, n draws)
 *
 * Returns the log-probability directly, always in log space.
 * This avoids the exp() → log() round-trip of the original code.
 * ------------------------------------------------------------------ */
static inline double C_log_dhyper(int x, int w, int b, int n) {
    if (x < 0 || x > w || x > n || (b + x) < n)
        return LOG_ZERO;
    return C_logChoose(w, x) + C_logChoose(b, n - x) - C_logChoose(w + b, n);
}

/* ------------------------------------------------------------------
 * C_dhyper – kept for backward compatibility with pmvhyper.c callers.
 * logp == 0  → returns probability (not log)
 * logp == 1  → returns log-probability
 * ------------------------------------------------------------------ */
static inline double C_dhyper(int x, int w, int b, int n, int logp) {
    double lp = C_log_dhyper(x, w, b, n);
    if (logp) return lp;
    return (lp == LOG_ZERO) ? 0.0 : exp(lp);
}

/* ------------------------------------------------------------------
 * log-sum-exp trick of computing the logarithm of the sum of two numbers, 
 * given that the inputs are already in logarithmic space: log_add(log_a, log_b) = log(exp(log_a) + exp(log_b))
 * Numerically stable; handles -Inf sentinel correctly.
 * ------------------------------------------------------------------ */
static inline double log_add(double log_a, double log_b) {
    if (log_a == LOG_ZERO) return log_b;
    if (log_b == LOG_ZERO) return log_a;
    if (log_a >= log_b)
        return log_a + log1p(exp(log_b - log_a));
    else
        return log_b + log1p(exp(log_a - log_b));
}
/*------------------------------------------------------------
 * log(1-exp(x)), x<=0
 * Numerically stable complement in log space.
 *------------------------------------------------------------*/
static inline double log1mexp(double x)
{
    /* domain check */
    if (x > 0.0)
        return NAN;

    /* log(1-exp(-log2)) = switch point */
    if (x > -0.69314718055994530942)
        return log(-expm1(x));
    else
        return log1p(-exp(x));
}

void C_dmvhyper(int *x, int *nL, int *L, int *n, double *p, int *logp, int *errorCode);
void C_pmvhyper(int *x, int *nL, int *L, int *n, double *p, int *lower, int *logp, int *errorCode);

#endif /* MVHYPER_H */
