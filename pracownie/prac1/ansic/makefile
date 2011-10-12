CFLAGS = $(shell pkg-config --cflags --libs glib-2.0)
GCC = gcc ${CFLAGS}



all:
	${GCC} pwz.c  -o pwz
	${GCC} pwzd.c -o pwzd
	${GCC} pwz_testser.c -o pwz_tester
all-d:
	${GCC} pwz.c  -o pwz -ggdb 
	${GCC} pwzd.c -o pwzd -ggdb 
	${GCC} pwz_testser.c -o pwz_tester -ggdb
