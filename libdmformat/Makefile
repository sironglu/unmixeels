CC = gcc

CFLAGS = -O2 -Wall -fPIC

LINK = -shared -fPIC

LIB = -L. -ldmformat 

objects-lib = libdm3format.o libdm4format.o libdmfilemap.o get_objs3.o get_objs4.o 

objects = $(objects-lib) dmshowinfo.o 

dmshowinfo: dmshowinfo.o libdmformat.so 
	$(CC) -o dmshowinfo dmshowinfo.o  $(LIB)

libdmformat.so : $(objects)
	$(CC) $(LINK)  -o libdmformat.so $(objects-lib)

dmshowinfo.o : libdmfilemap.h libdmformat.h dmshowinfo.c libdm3format.h libdm4format.h
	$(CC) $(CFLAGS) -o dmshowinfo.o -c dmshowinfo.c

libdmfilemap.o : libdmfilemap.h libdmfilemap.c
	$(CC) $(CFLAGS)  -o libdmfilemap.o -c libdmfilemap.c 

libdm3format.o : libdm3format.h libdmformat.c
	sed 's/DM_/DM3_/g' libdmformat.c >libdm3format.c
	$(CC) $(CFLAGS)  -DDMVERSION=3 -o libdm3format.o -c libdm3format.c 

libdm4format.o : libdm4format.h libdmformat.c
	sed 's/DM_/DM4_/g' libdmformat.c >libdm4format.c
	$(CC) $(CFLAGS)  -DDMVERSION=4 -o libdm4format.o -c libdm4format.c 

libdm3format.h : libdmformat.h
	sed 's/DM_/DM3_/g' libdmformat.h >libdm3format.h

libdm4format.h : libdmformat.h
	sed 's/DM_/DM4_/g' libdmformat.h >libdm4format.h

get_objs3.o : get_objs.c libdm3format.h
	sed 's/DM_/DM3_/g' get_objs.c >get_objs3.c
	$(CC) $(CFLAGS)  -DDMVERSION=3 -o get_objs3.o -c get_objs3.c 

get_objs4.o : get_objs.c libdm4format.h
	sed 's/DM_/DM4_/g' get_objs.c >get_objs4.c
	$(CC) $(CFLAGS)  -DDMVERSION=4 -o get_objs4.o -c get_objs4.c 

.PHONY : clean
clean :
	$(RM) $(objects) libdm3format.h libdm4format.h

