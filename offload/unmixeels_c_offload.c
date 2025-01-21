/*compile with
gcc -Wall -O2 -fPIC -o unmixeels_c_offload.so unmixeels_c_offload.c -shared -lm -lR 
*/

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

void my_gaussian_1(double* out, double *input, int *n, int *window, double *alpha, int *tails) {
	if ((*window)<=0) {
		memcpy(out, input, (*n)*sizeof(double));
	} else {
		double *core_func;
		*window = (*window + 1)/2*2-1;
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
		p = (*window - 1 ) / 2;
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

void img_x_drift_by_arr_nocut_f1(double *inImg, double *outImg, int *inDim, int *outDim, int *origShift, int *x_drift_arr_all_int, double *x_drift_arr_all, int *subpix) {
  int i,j;
  int nLines, inWidth, outWidth;
  int iint;
  double fint, val;
  nLines = inDim[1];
  inWidth = inDim[0];
  outWidth = outDim[0];
  if (*subpix) {
    for (i=0;i<nLines;i++) {
      iint = (int)round(x_drift_arr_all[i]);
      fint = x_drift_arr_all[i] - iint;
      for (j=0;j<inWidth;j++) {
        if (fint<0) {
          if (j==inWidth-1) 
            val = inImg[j + inWidth*i];
          else 
            val = (1+fint)*inImg[j+inWidth*i] - fint*inImg[ 1+j+inWidth*i];
        } else {
          if (j==0)
            val = inImg[j + inWidth*i];
          else
            val = (1-fint)*inImg[j+inWidth*i] + fint*inImg[-1+j+inWidth*i];
        }
        outImg[*origShift+iint+j + outWidth*i] = val;
      }
    }
  } else {
    for (i=0;i<nLines;i++) {
      for (j=0;j<inWidth;j++) {
        outImg[*origShift+x_drift_arr_all_int[i]+j + outWidth*i] = inImg[j + inWidth*i];
      }
    }
  }
}

void c_remove_spike_in_row_by_l_and_r(double *row, int *l, int *r, double *rowout, int *rowlen) {
  int i,ll,rr,d;
  memcpy(rowout, row, (*rowlen)*sizeof(double));
  ll = *l-1; rr = *r-1; d = rr-ll;
  if (d>=2) {
    for (i=ll+1;i<rr;i++){
      rowout[i] = (row[ll]*(rr-i) + row[rr]*(i-ll))/d;
    }
  }
}
    




