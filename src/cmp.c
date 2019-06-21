#include "mvhyper.h"
int max2(int a, int b)
{
   if(a>b) return(a);
   return(b);
}
int min2(int a, int b)
{
   if(a>b) return(b);
   return(a);
}
int max(int *a, int n)
{
	int i,result;
	result=a[0];
	for (i=1; i<n; i++){
		if (a[i]>result){
			result=a[i];
		}
	}
	return(result);
}
int min(int *a, int n)
{
	int i,result;
	result=a[0];
	for (i=1; i<n; i++){
		if (a[i]<result){
			result=a[i];
		}
	}
	return(result);
}
