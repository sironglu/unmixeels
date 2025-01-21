// nvcc --compiler-options '-fPIC -lcudart -shared -lm -lR ' unmixeels_cuda_offload.cu -lcudart -o unmixeels_cuda_offload.so

#include <stdio.h>

#define MAX_THREADS_PER_BLOCK 256

__global__
void cuda_ddot(float *image_drifted_device, float *cor_device, 
		int nElems) 
{
	if (0 == threadIdx.x) {
		float mean = 0;int i;
		for (i=0; i<nElems; i++) 
			mean += image_drifted_device[blockIdx.x*nElems+i];
		mean /= nElems;
		for (i=0; i<nElems; i++) 
			image_drifted_device[blockIdx.x*nElems+i] -= mean;
		cor_device[blockIdx.x] = rnormf ( nElems, &image_drifted_device[blockIdx.x*nElems] );
	}
}


__global__ 
void cuda_cor(float *image_drifted_device, float *cor_device, 
		int nLines, int nElems, int fixed_line) 
{
	int i, j;
	float sum;

	if (threadIdx.x < nElems && blockIdx.x != fixed_line) {
		i = blockIdx.x*nElems + threadIdx.x;
		j = fixed_line*nElems + threadIdx.x;
		image_drifted_device[i] *=image_drifted_device[j];
	}
	__syncthreads();
	sum = 0;
	if (0 == threadIdx.x && blockIdx.x != fixed_line) {
		for (i=0;i<nElems;i++) {
			j = blockIdx.x*nElems + i;
			sum += image_drifted_device[j];
		}
		cor_device[blockIdx.x] *=  sum*cor_device[fixed_line];
	}
}

extern "C" 
void img_cor(double *image_drifted, double *cor, int *img_dims, int *fixed_line)
{
	float *image_drifted_device;
	float *cor_device;
	float *image_drifted_f, *cor_f;
	int i, blocksize; 
	const int nLines = img_dims[1];
	const int nElems = img_dims[0];
	const int nPixes = nLines * nElems;
	const int img_size = nPixes*sizeof(float);
	const int cor_size = nLines*sizeof(float);

	cudaDeviceProp prop;
	cudaGetDeviceProperties(&prop, 0);

	if (nElems > prop.maxThreadsPerBlock) {
		printf("Error: correlation cannot be calculated for image with %d pixels per line. Max allowed number of pixels per line is %d.\n", nElems, prop.maxThreadsPerBlock);
	} else {
		image_drifted_f = (float *)malloc(img_size);
		for (i=0;i<nPixes;i++) {
			image_drifted_f[i] = image_drifted[i];
		}

		cudaMalloc( (void**)&image_drifted_device, img_size ); 
		cudaMalloc( (void**)&cor_device, cor_size ); 
		cudaMemcpy( image_drifted_device, image_drifted_f, img_size, cudaMemcpyHostToDevice ); 

		free(image_drifted_f);

		for (blocksize = 32; blocksize < nElems; blocksize*=2);
		dim3 dimBlock( blocksize, 1 );
		dim3 dimGrid( nLines, 1 );
		cuda_ddot<<<dimGrid, 1>>>(image_drifted_device, cor_device, 
						nElems);
		cuda_cor<<<dimGrid, dimBlock>>>(image_drifted_device, cor_device, 
						nLines, nElems, *fixed_line);
			
		cor_f = (float *)malloc(cor_size);
		cudaMemcpy( cor_f, cor_device, cor_size, cudaMemcpyDeviceToHost ); 
		cudaFree( image_drifted_device );
		cudaFree( cor_device );
		for (i=0;i<nLines;i++) {
			cor[i] = cor_f[i];
		}
		free(cor_f);
	}
}


__global__ 
void cuda_calc_kern(float *kern_func_device,
	int window, float mu, float sigma) 
{
	int i;
	__shared__ float scale;
	if (threadIdx.x < window ){
		kern_func_device[threadIdx.x] = 
			exp(-(threadIdx.x-mu)*(threadIdx.x-mu)/(2.*sigma*sigma));
	}
	__syncthreads();
	if (0 == threadIdx.x) {
		scale = 0;
		for (i=0;i<window;i++) {
			scale += kern_func_device[i];
		}
		scale = 1./scale;
	}
	__syncthreads();
	if (threadIdx.x < window ){
		kern_func_device[threadIdx.x] *= scale;
	}
}

__global__ 
void cuda_cp_ext(float *cnt_device, float *ext_device, 
	int nElems, int ext_nElems, int window) 
{
	int i,j,k,p;
	i = threadIdx.x + blockIdx.y * blockDim.x; 
	j = blockIdx.x*nElems;
	k = blockIdx.x*ext_nElems;
	p = (window - 1)/2;
	if (i<ext_nElems) {
		if (i < p) {
			ext_device[i+k] = cnt_device[j];
		} else if (i >= (nElems+p)){
			ext_device[i+k] = cnt_device[j+nElems-1];
		} else {
			ext_device[i+k] = cnt_device[i+j-p];
		} 
	}
}

__global__
void cuda_calc_smth(float *kern_func_device, float *ext_device, float *cnt_device, 
	int nElems, int ext_nElems, int window) 
{
	int i,j,k,p;
	float sum;
	i = threadIdx.x + blockIdx.y * blockDim.x; 
	j = blockIdx.x*nElems;
	k = blockIdx.x*ext_nElems;
	if (i < nElems) {
		sum = 0;
		for (p=0;p<window;p++) {
			sum += ext_device[i+p+k]*kern_func_device[p];
		}
		cnt_device[i+j] = sum;
	}
}


extern "C" 
void cuda_smth_cnt(double* cnt_out, double *cnt_in, int *cnt_dims, 
		int *window, double *alpha, int *tails) {
	float *cnt_f, *cnt_device, 
		*ext_device, *kern_func_device;
	int i, j, blocksize; 

	const int nLines = cnt_dims[1];
	const int nElems = cnt_dims[0];
	const int nPixes = nLines * nElems;
	const int cnt_size = nPixes*sizeof(float);
	const int kern_size = (*window)*sizeof(float);
	const int ext_nElems = *window+nElems;
	const int ext_nPixes = nLines * ext_nElems;
	const int ext_cnt_size = ext_nPixes*sizeof(float);

	float sigma, mu;

	if ((*window)<=0) {
		memcpy(cnt_out, cnt_in, nPixes*sizeof(double));
	} else {
		*window = (*window + 1)/2*2-1;
		sigma = .5 * (*window) / (*alpha);

		//Is there a better way when window is an even number?
		mu = floor((*window) / 2.);

		for (blocksize = 32; blocksize < *window; blocksize*=2);
		dim3 dimBlock( blocksize, 1 );
		dim3 dimGrid( nLines, 1 );
		cudaMalloc( (void**)&kern_func_device, kern_size ); 
		cuda_calc_kern<<<1, dimBlock>>>(kern_func_device, *window, 
					mu, sigma);
		cnt_f = (float *)malloc(nPixes*sizeof(float));
		for (i=0;i<nPixes;i++) 
			cnt_f[i] = cnt_in[i];

		cudaMalloc( (void**)&cnt_device, cnt_size ); 
		cudaMalloc( (void**)&ext_device, ext_cnt_size ); 

		cudaMemcpy( cnt_device, cnt_f, cnt_size, cudaMemcpyHostToDevice );

		j = ext_nElems < MAX_THREADS_PER_BLOCK ? ext_nElems : MAX_THREADS_PER_BLOCK;
		for (blocksize = 32; blocksize < j; blocksize*=2);
		dimBlock.x = blocksize;
		dimGrid.y = ext_nElems / MAX_THREADS_PER_BLOCK + 1;
		cuda_cp_ext<<<dimGrid, dimBlock>>>(cnt_device, ext_device, 
			nElems, ext_nElems, *window);
		j = nElems < MAX_THREADS_PER_BLOCK ? nElems : MAX_THREADS_PER_BLOCK;
		for (blocksize = 32; blocksize < j; blocksize*=2);
		dimBlock.x = blocksize;
		dimGrid.y = nElems / MAX_THREADS_PER_BLOCK + 1;
		cuda_calc_smth<<<dimGrid, dimBlock>>>(kern_func_device, ext_device, cnt_device, 
			nElems, ext_nElems, *window);
		cudaMemcpy( cnt_f, cnt_device, cnt_size, cudaMemcpyDeviceToHost ); 
		cudaFree( cnt_device );
		cudaFree( ext_device );
		cudaFree( kern_func_device );
		for (i=0;i<nPixes;i++) 
			cnt_out[i] = cnt_f[i];
		free(cnt_f);
	}
}


