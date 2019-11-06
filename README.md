unmixeels
===

## Usage

### Install

1. Install `gcc`, `make` and `R` on your Linux system. 
   On Debian/Ubuntu, run
   ```sudo apt-get install build-essential r-base```
2. Install `R6` library for R. 
   Run R from terminal, then run the following command:
   ```> install.packages("R6")```
   You may also want the "plotrix" library: 
   ```> install.packages("plotrix")```
2. Download the source package.
3. Compile `libdmformat` :
   ```cd libdmformat ; make```
4. Compile subroutine `my_gaussian_1.c` :
```
   cd .. 
   gcc -Wall -O2 -fPIC -o my_gaussian_1.so my_gaussian_1.c -shared -lm -lR
```

### Usage

1. Copy your dm3/dm4 files to the same directory as the `simulated_data.R`
2. Modify the value of `FILENAME*` variables accordingly, or use the simulated data. 
3. Modify other parameters in the source code accordingly. 
4. Copy and paste the source code into your `R` terminal. 

