#include "mvhyper.h"
void C_pmvhyper(int *x, int *nL, int *L, int *n, double *p, int *lower, int *logp, int *errorCode){
	/*
	x:     number of elements overlap between all subsets
	nL:    number of subsets
	L:     subset sizes
	n:     background size
	p:     output probability
	lower: 1, lower tail probability Pr(overlap <= x); 0, upper tail probability Pr(overlap > x)
	logp:  return log probability
	*/

    int obs      = *x;
    int t        = *nL;
    int N        = *n;
    int minL     = min(L, t);

    int wantLower = *lower;
    int wantLog   = *logp;

    int logFlag = 1;
    double logTail = LOG_ZERO;
	double log_epsilon = -36.84; // Equivalent to a relative threshold of 1e-16 (machine epsilon)
    double tmp;

    /*-------------------------------
      Boundary cases
    -------------------------------*/

    if (obs < 0) {
        *p = wantLog ?
             (wantLower ? LOG_ZERO : 0.0) :
             (wantLower ? 0.0 : 1.0);
        return;
    }

    if (obs > minL) {
        *p = wantLog ?
             (wantLower ? 0.0 : LOG_ZERO) :
             (wantLower ? 1.0 : 0.0);
        return;
    }

	/*-------------------------------
      Compute expected overlap
    -------------------------------*/

    double mean = (double)L[0];

	for (int i = 1; i < t; i++)
    	mean *= (double)L[i] / N;

    /*-------------------------------
      Choose shorter tail
    -------------------------------*/

    int from, to;
    int summedLower;

    if (obs <= mean) {
        from = 0;
        to = obs;
        summedLower = 1;
    } else {
        from = obs + 1;
        to = minL;
        summedLower = 0;
    }

    /*-------------------------------
      Accumulate chosen tail
    -------------------------------*/
    for (int k = from; k <= to; k++) {
		if(summedLower > 0){
			int k2 = obs - k;
        	C_dmvhyper(&k2, nL, L, n, &tmp, &logFlag, errorCode);
			// C_dmvhyper_log(&k2, nL, L, n, &tmp, &logFlag, errorCode, logVal);
		}else{
			C_dmvhyper(&k, nL, L, n, &tmp, &logFlag, errorCode);
			// C_dmvhyper_log(&k, nL, L, n, &tmp, &logFlag, errorCode, logVal);
		}
		if (*errorCode) {
            return;
        }
		if(k > from){//No improve in precision
			if(tmp < tiny) break;
			if (tmp - logTail < log_epsilon) break;
		}
        logTail = log_add(logTail, tmp);
    }

    /*-------------------------------
      Convert to requested tail
    -------------------------------*/

    double logResult;

    if (summedLower == wantLower)
        logResult = logTail;
    else
        logResult = log1mexp(logTail);

    /*-------------------------------
      Return
    -------------------------------*/

    if (wantLog)
        *p = logResult;
    else
        *p = (logResult == LOG_ZERO) ? 0.0 : exp(logResult);
}
