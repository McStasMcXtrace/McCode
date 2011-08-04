// This is a 'support routine' used by the nedriv.f code.  In brief, this
// is the main interface between the Fortran and C languages.  It is
// called from Fortran and uses an Objective C speaker object to send
// messages to the PGPLOT viewer.
//
#import "pgvSpeaker.h"
#import <stdio.h>

void mkspeak( id * ispeak);
id   mySpeak=NULL;

#ifdef ABSOFT
void NEXSUP ( int *ifunc,   char *cbuf,   float rtmp[],
           int len_ifunc, int len_cbuf, int len_rtmp)
#else
void nexsup_( int *ifunc, char *cbuf, float rtmp[], int len_cbuf)
#endif
{
      int n1, n2, itmp;
      double d1, d2;

      if(mySpeak==NULL) {
         mkspeak(&mySpeak);
      }
         
      switch (*ifunc) {
      case 1:
         [mySpeak getwind: &n1 by: &n2 scale:&d1 color: &itmp];
         rtmp[0]= (float) n1;
         rtmp[1]= (float) n2;
         rtmp[2]= (float) d1;
         rtmp[3]= (float) itmp;
         break;
      case 2:
         [mySpeak beginp];
         break;
      case 3:
         [mySpeak pscode:cbuf];
         break;
      case 4:
         [mySpeak cursorat: &d1 and: &d2 char: &itmp];
         rtmp[0]= (float) d1;
         rtmp[1]= (float) d2;
         rtmp[2]= (float) itmp;
         break;
      case 5:
         [mySpeak flush];
         break;
      case 6:
         [mySpeak endp];
         break;
      case 7:
         [mySpeak free];
         mySpeak=NULL;
         break;
      default :
         printf("nexsup--Unknown function code= %d\n",*ifunc);
         break;
      }
}

#import "mkspeak.m"

#import "pgvSpeaker.m"
