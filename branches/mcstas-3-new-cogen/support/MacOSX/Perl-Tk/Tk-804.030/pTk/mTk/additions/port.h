/*
 * Warning, this file was automatically created by the TIFF configure script
 * VERSION:	 v3.5.5
 * DATE:	 Sun Jul 16 20:21:17 MET DST 2000
 * TARGET:	 i386-unknown-linux
 * CCOMPILER:	 /usr/bin/gcc-2.7.2.1
 */
#ifndef _PORT_
#define _PORT_ 1
#ifdef __cplusplus
extern "C" {
#endif
#include <sys/types.h>
#define HOST_FILLORDER FILLORDER_LSB2MSB
#define HOST_BIGENDIAN	0
#define HAVE_MMAP 1
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
typedef double dblparam_t;
#ifdef __STRICT_ANSI__
#define	INLINE	__inline__
#else
#define	INLINE	inline
#endif
#define GLOBALDATA(TYPE,NAME)	extern TYPE NAME
#ifdef __cplusplus
}
#endif
#endif
