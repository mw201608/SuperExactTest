#include "mvhyper.h"

double C_logChoose(int n, int k){
//log binomial coefficient nCk
	int i;
	int m=min2(k,n-k);
	double result=0.0;
	if(m==0) return result;
	for(i=1;i <= m; i++)
		result=result+log((double)(n-i+1))-log((double)i);
	return result;
}
double C_logChoose_logVal(int n, int k, double *logVal){
//log binomial coefficient nCk
	int i;
	int m=min2(k,n-k);
	double result=0.0;
	if(m==0) return result;
	for(i=0;i < m; i++){
		result=result+logVal[n-i-1]-logVal[i];
	}
	return result;
}
