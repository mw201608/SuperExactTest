#include "mvhyper.h"
void C_dmvhyper(int *x, int *nL, int *L, int *n, double *p, int *logp, int *errorCode) {
	/*
	x:     number of elements overlap between all subsets
	nL:    number of subsets
	L:     subset sizes
	n:     background size
	p:     output probability
	logp:  return log probability
	*/
    int obs  = *x;
    int t    = *nL;
    int N    = *n;
	int minL = min(L, t);

	int i, j, k, l;
	int aSize = max(L,t) - obs + 1;
	double *f1, *f0;
	double temp;

    /* ── trivial cases ────────────────────────────────────────────────── */
    if (obs < 0 || obs > minL) {
        *p = (*logp) ? LOG_ZERO : 0.0;
        return;
    }
    if (t == 2) {
        *p = C_dhyper(obs, L[0], N - L[0], L[1], *logp);
        return;
    }

	f1 = malloc(sizeof(double)*aSize);
	f0 = malloc(sizeof(double)*aSize);
	if (!f1 || !f0) {
		free(f1);
		free(f0);
		*errorCode = 1;
		return;
	}
	for(i = 0; i < aSize; i++) f1[i] = LOG_ZERO;
	//from inner-most to outer-most
	l = obs;
	f1[0] = C_log_dhyper(obs, l, N - l, L[t - 1]);
	for(l = obs + 1; l <= min2(minL,N + obs - L[t - 1]); l++){ //f1(l) = f1(l-1) * (n-l+1-L[nL-1]+x)/(l-x) * l/(n-l+1)
		f1[l - obs] = f1[l - obs -1] + log((double)(N - l + 1 - L[t - 1] + obs) / (double)(l - obs) * (double)l / (double)(N - l + 1));
	}
	memcpy ( f0, f1, aSize * sizeof((double) 0) );
	for(i = 2; i <= t - 2; i++){
		for(k = obs; k <= minL; k++){ //calculate f_l(k)
			l = max2(obs, k + L[t - i] - N);
			temp = C_log_dhyper(l, k, N -k, L[t - i]);
			f1[k - obs] = temp + f0[l - obs];
			for(l = l + 1; l <= k; l++){ //sum over l for each k
				temp = temp + log((double)(L[t - i]-l+1) / (double)(l) * (double)(k-l+1) / (double)(N - L[t - i]-k+l));
				f1[k - obs] = log_add(f1[k - obs], temp + f0[l - obs]);
			}
		}
		memcpy ( f0, f1, aSize * sizeof((double) 0) );
	}
	//final integration
	j = max2(obs, L[1]+L[0] - N);
	temp = C_log_dhyper(j, L[1], N - L[1], L[0]);
	double log_p = temp + f1[j - obs];
	for(j=j+1;j <= minL;j++){
		temp = temp + log((double)(L[1]-j+1) / (double)(j) * (double)(L[0]-j+1) / (double)(N - L[1]-L[0]+j));
		log_p = log_add(log_p, temp + f1[j - obs]);
	}
    double prob;
    if (log_p == LOG_ZERO || isinf(log_p)) {
		if (*logp){
			prob = LOG_ZERO;
		}else{
        	prob = 0.0;
		}
    } else {
		if(log_p > 0.0) log_p = 0.0;
		if (*logp){
			prob = log_p;
		}else{
			prob = exp(log_p);
		}
    }
	*p = prob;
	free(f1);
	free(f0);
	return;
}
