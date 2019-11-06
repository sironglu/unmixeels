/*compile with
gcc -Wall -O2 -fPIC -o my_gaussian_1.so my_gaussian_1.c -shared -lm -lR 
*/

#include <math.h>
#include <string.h>
#include <stdlib.h>
//#include <stdio.h>

//(Tiyo00$y,window=smth_window, alpha=smth_alpha, tails=TRUE)
void my_gaussian_1(double* out, double *input, int *n, int *window, double *alpha, int *tails) {
	if ((*window)<=0) {
		memcpy(out, input, (*n)*sizeof(double));
	} else {
		double *core_func;
		core_func = malloc(sizeof(double)*(*window));
		double sigma;
		sigma = .5 * (*window) / (*alpha);
//		printf("%d",*window);
		double mu;
		//Is there a better way when window is an even number?
		mu = floor((*window) / 2.);
		int i,j;
		for (i=0;i<(*window);i++) {
			core_func[i] = exp(-(i-mu)*(i-mu)/(2.*sigma*sigma))/(sigma*sqrt(2*M_PI));
		}
		double scale;
		for ((i=0,scale=0);i<(*window);i++) {
			scale+=core_func[i];
		}
		for (i=0;i<(*window);i++) {
			core_func[i]/=scale;
		}	
		double *in_ext;
		in_ext = malloc(sizeof(double)*(*window+*n));
		int p;
		p = (*window) / 2;
		memcpy(&(in_ext[p]), input, (*n)*sizeof(double));
		if (*tails) {
			for (i=0;i<p;i++) {
				in_ext[i] = input[0];
			}
			for (i=(p+(*n));i<(*window+*n);i++) {
				in_ext[i] = input[(*n)-1];
			}
		} else {
			memset(in_ext, 0, p*sizeof(double));
			memset(&(in_ext[p+(*n)]), 0 , (*window - p)*sizeof(double));
		}
		for (i=0;i<(*n);i++) {
			out[i] = 0.;
			for (j=0;j<(*window);j++) {
				out[i] += in_ext[i+j]*core_func[j];
			}
		}
		free (in_ext);
		free(core_func);
	}
}

