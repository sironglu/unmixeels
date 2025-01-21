# if DMVERSION == 4
#  include "libdm4format.h"

#ifndef UNITS_BUFSIZE
#define UNITS_BUFSIZE 20
#endif

#include <R.h>
#include <Rdefines.h>

/* declear */
int DM_get_image2(DM_node** root, int* n_ImageTag, char** with_name, char** Units, int* dimension_len, void* data, int* copy_data);


void xp2char(SEXP chars, char *s)
{
   int n, i;

   n = length(chars);
   if (s != NULL) {
      for (i = 0; i < n; i++) {
         s[i] = *CHAR(STRING_ELT(chars, i));
         printf("%d",n);
      }
      s[n] = '\0';
   } else {
      printf("Error: SEXP to char conversion failed. \n");
   }
}

void char2xp (SEXP chars, char *s)
{
  int n, i;
  
  n = length(chars);
  for (i=0; (s[i]!='\0') && (i < n) ; i++)
    SET_STRING_ELT(chars, i, mkChar(&s[i]));
  SET_STRING_ELT(chars, i, mkChar('\0'));
}

SEXP DM_get_image2_4gb(SEXP R_root, SEXP R_n_ImageTag, SEXP R_with_name, SEXP R_Units, SEXP R_dimension_len, SEXP R_data, SEXP R_data_type, SEXP R_copy_data) {
  char* with_name; char** Units;
  void* tmpdata;
  int64_t i;
  //with_name = malloc(sizeof(char)*UNITS_BUFSIZE);
  //xp2char(R_with_name, with_name);
  with_name = (char*)CHAR(STRING_ELT(R_with_name, 0));
  Units = malloc(sizeof(char *)*(*INTEGER(R_dimension_len)));
  int n_ImageTag = *INTEGER(R_n_ImageTag);
  //char2xp(R_Units, Units);
  for (i=0;i<length(R_Units);i++) {
    Units[i] = malloc(sizeof(char)*UNITS_BUFSIZE);
  }

  if (*INTEGER(R_data_type) == 7) 
    {}// tmpdata = malloc(sizeof(double)*xlength(R_data));
  else if (*INTEGER(R_data_type) == 6) 
    {}// tmpdata = malloc(sizeof(float)*xlength(R_data));
  else 
    printf("Error: data type %d not implemented in DM_get_image2_4gb(). ", *INTEGER(R_data_type));
    int copy_data_image2 = 0;
    DM_get_image2(RAW(R_root), &n_ImageTag, &with_name, Units, INTEGER(R_dimension_len), &tmpdata, &copy_data_image2);
  for (i=0;i<length(R_Units);i++) {
    SET_STRING_ELT(R_Units, i, mkChar(Units[i]));
  }
  if (*INTEGER(R_copy_data)) {
    for (i=0;i<xlength(R_data);i++) {
      if (*INTEGER(R_data_type) == 6) {
        REAL(R_data)[i] = ((float *)tmpdata)[i];
      } else {
        REAL(R_data)[i] = ((double *)tmpdata)[i];
      }
    }
  } else {
    char *tmpdataaddr = CHAR(asChar(R_data));
    sprintf(tmpdataaddr, "0x%016lx", (uint64_t)tmpdata);
    printf("Info: DM_get_image2_4gb: data is not copied, address is %s\n", tmpdataaddr);
    //R_data = PROTECT(mkString(tmpdataaddr));
  }  
//  free(tmpdata);
//  free(with_name);
  for (i=0;i<length(R_Units);i++) {
    free(Units[i]);
  }
  free(Units);
  //UNPROTECT(1);
  return(R_NilValue);
}

# endif

