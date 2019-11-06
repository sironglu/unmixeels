#include <stdio.h>
#include <wchar.h>
#include <inttypes.h>
#include <locale.h>
#include <iconv.h>
#include <string.h>

int test(char** a){
	printf("%p %p %p\n", a, a[0], a[1]);
	char* b; char* c;
	b = *a;c = (*a)+1;
	printf("%p %p %p\n", a, a[0], a[1]);
	printf("%c %c\n", *b, *c);
	printf("%c %c\n", *(char*)(a[0]), *(char*)(a[1]));
	printf("%s %s %p\n", (char*)(a[0]), (char*)(a[1]), *(char*)a[2]);
	return 0;
}

int test2(char** a){
	strcpy(a[0],"fdsa");
	strcpy(a[1],"qwe");
	return 0;
}

#define _UNICODE
#define UNICODE
#include <locale.h>

int main(){
	char a[6];
	char b[12];
	a[0] = 'c';
	a[1] = 0;
	a[2] = 'a';
	a[3] = 0;
	a[5] = 0;
	a[6] = 0;
//	printf("%s\n",a);
//	wprintf(L"%c\n", L'ρ');
//	wprintf(L"%c\n", *(uint16_t*)(a));
    if (!setlocale(LC_CTYPE, "")) {
      fprintf(stderr, "Can't set the specified locale! "
              "Check LANG, LC_CTYPE, LC_ALL.\n");
      return 1;
    } else {
         setlocale(LC_ALL, "");
	iconv_t cd;
	char *inptr = a;
	char *wrptr = b;
	size_t insize = 4;
	size_t avail = 11;
      size_t nconv;
	cd = iconv_open ("", "UCS-2");
	if (cd == (iconv_t) -1) {
		perror("iconv_open");
	} else {
		nconv = iconv (cd, &inptr, &insize, &wrptr, &avail);
	}

    }
	printf("%s", b);
/*	printf("%d %d %d %d\n", sizeof(wchar_t), sizeof(L'Δ'),sizeof('Δ'),sizeof('d'));
    printf("%s\n", "ο Δικαιοπολις εν αγρω εστιν");
	printf("%lc\n", L'Δ');
	int fdsa = L'Δ';
	printf("%lc", fdsa);
*/
	return 0;
}






