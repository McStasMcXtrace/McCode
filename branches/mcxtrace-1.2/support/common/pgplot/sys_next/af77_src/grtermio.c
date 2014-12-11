/* Support routines for terminal I/O. This module defines the following
   Fortran-callable routines: GROTER, GRCTER, GRWTER, GRRTER.
   Abosft FORTRAN callable version for NeXT */

#include <sys/ioctl.h>

int GROTER(cdev, ldev, w_cdev, w_ldev)
char *cdev;
long int *ldev;
int w_cdev, w_ldev;

/* Open a channel to the device specified by 'cdev'.
 *
 * cdev      I    The name of the device to be opened
 * ldev      I    Number of valid characters in cdev
 * groter      O  The open channel number (-1 indicates an error)
 */
{
    int fd, n;
    char name[64];

    n = *ldev;
    if (n > 63)
        n = 63;
    strncpy(name, cdev, n);
    name[n] = '\0';
    if ((fd = open(name, 2)) == -1)
        {
/*      perror("Cannot access graphics device");
 */
        perror(name);
        return -1;
        }
    else
        {
        return fd;
        }
}

int GRCTER(fd, w_fd)
int *fd;
int w_fd;

/* Close a previously opened channel.
 *
 * fd        I    The channel number to be closed
 */
{
    close(*fd);
}

GRWTER(fd, cbuf, lbuf, w_fd, w_cbuf, w_lbuf)
int *fd;
char *cbuf;
long int *lbuf;
int w_fd, w_cbuf, w_lbuf;

/* Write lbuf bytes from cbuf to the channel fd.  Data is written in
 * CBREAK mode.
 *
 * fd        I    The channel number
 * cbuf      I    Character array of data to be written
 * lbuf      I/O  The number of bytes to write, set to zero on return
 */
{
    int nwritten;
    struct sgttyb tty;
    int save_flags;

/*    printf ("writing %d bytes on unit %d\n", *lbuf, *fd);  */

    ioctl(*fd, TIOCGETP, &tty);
    save_flags = tty.sg_flags;
    tty.sg_flags |= CBREAK;
    ioctl(*fd, TIOCSETP, &tty);
    tty.sg_flags = save_flags;

    nwritten = write (*fd, cbuf, *lbuf);
    ioctl(*fd, TIOCSETP, &tty);
    if (nwritten != *lbuf)
        perror("Error writing to graphics device");
    *lbuf = 0;
    return;
}

GRPTER(fd, cprom, lprom, cbuf, lbuf, w_fd, w_cprom, w_lprom, w_cbuf, w_lbuf)
int  *fd;
char *cprom, *cbuf;
long int *lprom, *lbuf;
int  w_fd, w_cprom, w_lprom, w_cbuf, w_lbuf;

/* Write prompt string on terminal and then read response.  This version
 * will try to read lbuf characters.
 *
 * fd        I    The channel number
 * cprom     I    An optional prompt string
 * lprom     I    Number of valid characters in cprom
 * cbuf        O  Character array of data read
 * lbuf      I/O  The number of bytes to read, on return number read
 */
{
    int i0, nread, ntry;
    struct sgttyb tty;
    int save_flags;

    ioctl(*fd, TIOCGETP, &tty);
    save_flags = tty.sg_flags;
    tty.sg_flags |= CBREAK;
    ioctl(*fd, TIOCSETP, &tty);
    tty.sg_flags = save_flags;

    if( *lprom>0)
      write (*fd, cprom, *lprom);
    i0=0;
    ntry=*lbuf;
    do {
        nread = read (*fd, &cbuf[i0], ntry);
 /*   printf("Nread=%d, Ntry=%d\n",nread,ntry); */
        i0=i0+nread;
        ntry=*lbuf-i0-1;
    } while (nread>0 && ntry>0);

    ioctl(*fd, TIOCSETP, &tty);
    *lbuf=i0;
    return;
}
