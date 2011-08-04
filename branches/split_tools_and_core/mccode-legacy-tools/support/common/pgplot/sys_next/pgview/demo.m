#include <stdio.h>
#include <stdlib.h>

void main(int argc, char *argv[])
{
      void nexsup_( int *ifunc, char *cbuf, float *rtmp);
      char *cbuf=NULL;
      float rtmp[20];
      int  ifunc;

      ifunc=1;
      printf("demo--Sending ifunc=1 showwind/getsize.\n");
      nexsup_(&ifunc, cbuf, rtmp);
      printf("demo-window size is %f %f %f\n",rtmp[0],rtmp[1],rtmp[2]);

      ifunc=2;
      rtmp[0]=1.;
      printf("demo--Sending ifunc=2 beginp/clear\n");
      nexsup_(&ifunc, cbuf, rtmp);

      ifunc=3;
      printf("demo--Sending ifunc=3, pscode.\n");
      nexsup_(&ifunc,
".20 setgray newpath 50 50 moveto 100 500 lineto stroke", rtmp);

      ifunc=5;
      printf("demo--Sending ifunc=5, flush\n");
      nexsup_(&ifunc, cbuf, rtmp);

      ifunc=3;
      printf("demo--Sending ifunc=3, pscode.\n");
      nexsup_(&ifunc,
"newpath 100 100 moveto 100 0 rlineto 0 100 rlineto 
-100 0 rlineto closepath eofill", rtmp);

      ifunc=3;
      printf("demo--Sending ifunc=3, pscode.\n");
      nexsup_(&ifunc,"newpath 0 0 moveto 20 20 lineto stroke", rtmp);

/*
      ifunc=3;
      printf("demo--Sending ifunc=3, userpath.\n");
      nexsup_(&ifunc,
"[ [ 0 0 200 200 175 100 200 100 100 175 100 200 25 100 0 100 100 25 100 0
] <000103010301030103> 
] ustroke",
      rtmp);
*/

      ifunc=5;
      printf("demo--Sending ifunc=5, flush\n");
      nexsup_(&ifunc, cbuf, rtmp);

/*
      ifunc=4;
printf("demo--Sending ifunc=4 read cursor.\n");
      nexsup_(&ifunc, cbuf, rtmp);
printf("demo--Cursor= %f %f %f\n",rtmp[0],rtmp[1],rtmp[2]);
*/

      exit(0);
}
