/* From: Stephen Green <green@nrccsb6.di.nrc.ca> */
/* To: tjp@deimos.caltech.edu */

#include <gl.h>
#include <gl/device.h>

/************************************************************************
 *
 *  irdriv -- IRIS4D PGPLOT Driver
 *
 *      Stephen J. Green -- Aug. 16, 1990 National Research Council of Canada
 *
 *	This driver handles the following capabilities:
 *
 *	arbitrary sized view surface
 *	32 colors, 16 pre-defined
 *	white background
 *	buffering
 *	a fully working cursor, with values returned for the mouse buttons
 *	fast polygon and rectangle fill
 *
 *	The device can be specfied by:
 *
 *	"/IRIS"		        --> 7.8" x 7.8" window (640x640)
 *	"X/IRIS"		--> X" x X" window
 *	"X,Y/IRIS"		--> X" x Y" window
 *
 *	X and Y are floating or integers, and have intelligent defaults.
 *
 */

/*
 *  dev_name -- the PGPLOT device name
 *
 */
static char dev_name[] = "IRIS  (SiliconGraphics Console)" ;
/*
 *  SCALE -- the number of pixels per inch
 *
 *  This value is only good for typical Sun workstations, and will
 *  result in a teeny window on high resolution workstations.  Suns
 *  are also slightly non-square, resulting in the X and Y scales
 *  each being off from 82 by about 1 pixel in opposite directions.
 *
 */
#define SCALE		92
/*
 *  DEF_*_WID -- default size of the screen
 *
 */
#define DEF_X_WID	640
#define DEF_Y_WID	640
/*
 *  MAX_*_WID -- maximum size of the screen
 *
 */
#define MAX_X_WID	1024
#define MAX_Y_WID	1024
/*
 *  MIN_*_WID -- minimum size of the screen
 *
 */
#define MIN_X_WID	50
#define MIN_Y_WID	50
/*
 *  *_WID -- size of the screen
 *
 */
int X_WID = DEF_X_WID ;
int Y_WID = DEF_Y_WID ;

static int COLOR = 1 ;			/* current pen color */
static int APPEND = 0 ;			/* to erase or not */

/*
 *  NCOLOR -- number of colors
 *
 *	PGPLOT predefines 16 colors, and suntools uses the last color
 *	for the cursor color, so the next factor of two (32) was used.
 *
 */

#define NCOLOR	32

/*  The pre-defined dark grey color is too dark */

static unsigned char R_table[NCOLOR] =
	{  1,255,255,  0,  0,  0,255,255, 255,  0,  0,  0,127,255, 85,170,
	   0,  0,  0,  0,  0,  0,  0,  0,   0,  0,  0,  0,  0,  0,  0,255 } ;
static unsigned char G_table[NCOLOR] =
	{  1,255,  0,255,  0,255,  0,255, 127,142,255,127,  0,  0, 85,170,
	   0,  0,  0,  0,  0,  0,  0,  0,   0,  0,  0,  0,  0,  0,  0,255 } ;
static unsigned char B_table[NCOLOR] =
	{  1,255,  0,  0,255,255,255,  0,   0,  0,127,255,255,127, 85,170,
	   0,  0,  0,  0,  0,  0,  0,  0,   0,  0,  0,  0,  0,  0,  0,255 } ;

/*
 *  These variables are used for the gl polygon fill routine
 *
 */
static long NPOLY = 0, POLY = 0 ;
static Icoord VERTEX[1024][2];

unsigned char GR_cursor() ;		/* get character at cursor */

void irdriv_( FUNC, BUFFER, NBUF, STRING, NSTR, len )
int *FUNC ;				/* function */
float *BUFFER ;				/* floating data */
int *NBUF ;				/* length of BUFFER */
char *STRING ;				/* character data */
int *NSTR ;				/* length of STRING */
int len ;				/* string length */
{
    int i ;
    int xpos, ypos ;
    float REQ_X_WID, REQ_Y_WID ;	/* requested widths */

    *NBUF = 0 ;
    *NSTR = 0 ;

    switch( *FUNC ) {
/*
 *************** return device name *************************************
 */
    case 1:
	strcpy( STRING, dev_name ) ;
	*NSTR = strlen( dev_name ) ;
	for( i = *NSTR ; i < len ; i++ )
	    STRING[i] = ' ' ;
	break ;
/*
 *************** return minimum range of view surface and color index ***
 */
    case 2:
	BUFFER[0] = 0 ;			/* minimum X value */
	BUFFER[1] = X_WID - 1 ;		/* maximum X value */
	BUFFER[2] = 0 ;			/* minimum Y value */
	BUFFER[3] = Y_WID - 1 ;		/* maximum Y value */
	BUFFER[4] = 0 ;			/* minimum color value */
	BUFFER[5] = NCOLOR - 1 ;	/* maximum color value */
	*NBUF = 6 ;
	break ;
/*
 *************** return device scale ************************************
 */
    case 3:
	BUFFER[0] = SCALE ;		/* X units per inch */
	BUFFER[1] = SCALE ;		/* Y units per inch */
/*
 *  The true pen with is, of course, 1 pixel, or 1.0 in device
 *  coordinates, but this fails miserably for PGPLOT.  A smaller
 *  pen width helps somewhat, but I have not twiddled this parameter
 *  enough to find the best value.  the /CGI interface has similar
 *  problems.
 *
 */
					/* should really be 1.0 */
	BUFFER[2] = 0.2 ;		/* pen width */
	*NBUF = 3 ;
	break ;
/*
 *************** return device capabilities *****************************
 */
    case 4:
	STRING[0] = 'I' ;		/* interactive device */
	STRING[1] = 'C' ;		/* cursor is available */
	STRING[2] = 'D' ;		/* no dashed lines */
	STRING[3] = 'A' ;		/* polygon fill available */
	STRING[4] = 'T' ;	/* T */	/* fat lines */
	STRING[5] = 'R' ;		/* rectangle fill available */
	STRING[6] = 'N' ;		/* not used */
        STRING[7] = 'V' ;		/* image lost on exit */
	STRING[8] = 'N' ;		/* not used */
	STRING[9] = 'N' ;		/* not used */
	*NSTR = 10 ;
	break ;
/*
 *************** return default device/file name ************************
 */
    case 5:
	STRING[0] = ' ' ;		/* no default name */
	break ;
/*
 *************** return default size of view ****************************
 */
    case 6:
	BUFFER[0] = 0 ;			/* default X min */
	BUFFER[1] = X_WID - 1 ;		/* default X max */
	BUFFER[2] = 0 ;			/* default Y min */
	BUFFER[3] = Y_WID - 1 ;		/* default Y max */
	*NBUF = 4 ;
	break ;
/*
 *************** return miscellaneous defaults **************************
 */
    case 7:
	BUFFER[0] = 2.0 ;		/* return a random number */
	*NBUF = 1 ;
	break ;
/*
 *************** select device ******************************************
 */
    case 8:
	/* do nothing */
	break ;
/*
 *************** open workstation ***************************************
 */
    case 9:
	BUFFER[0] = 0.0 ;		/* return channel 0 */
	BUFFER[1] = 1.0 ;		/* always successful open */
	if( BUFFER[2] )			/* no-erase mode */
	    APPEND = 1 ;
	else
	    APPEND = 0 ;
	*NBUF = 2 ;
	i = sscanf( STRING, "%f,%f", &REQ_X_WID, &REQ_Y_WID ) ;
	if( i == 1 ) {
	    REQ_Y_WID = REQ_X_WID ;
	    i = 2 ;
	    }
	if( i == 2 ) {
	    REQ_X_WID *= SCALE ;	/* scale inches to pixels */
	    REQ_Y_WID *= SCALE ;	/* scale inches to pixels */
	    if( REQ_X_WID < MIN_X_WID ) REQ_X_WID = DEF_X_WID ;
	    if( REQ_X_WID > MAX_X_WID ) REQ_X_WID = MAX_X_WID - MIN_X_WID ;
	    if( REQ_Y_WID < MIN_Y_WID ) REQ_Y_WID = DEF_Y_WID ;
	    if( REQ_Y_WID > MAX_Y_WID ) REQ_Y_WID = MAX_Y_WID - MIN_Y_WID ;
	    X_WID = REQ_X_WID ;
	    Y_WID = REQ_Y_WID ;
	    }

	GR_start() ;
	break ;
/*
 *************** close workstation **************************************
 */
    case 10:
	break ;
/*
 *************** begin picture ******************************************
 */
    case 11:
	concave(TRUE);
	if( !APPEND )			/* erase screen */
	    reshapeviewport();
	    RGBcolor(0,0,0);      /* set to black */
	    clear();
	    /* set back to current color */
	    RGBcolor(R_table[COLOR],G_table[COLOR],B_table[COLOR]);
	break ;
/*
 *************** draw line **********************************************
 */
    case 12:
	move2i((Icoord) BUFFER[0],(Icoord) BUFFER[1]);
	draw2((Icoord) BUFFER[2],(Icoord) BUFFER[3]);
	break ;
/*
 *************** draw dot ***********************************************
 */
    case 13:
	pnt2((Icoord) BUFFER[0], (Icoord) BUFFER[1]);
	break ;
/*
 *************** end picture ********************************************
 */
    case 14:
#ifdef CORRECT_END
	if( BUFFER[0] )
	    reshapeviewport();
	    RGBcolor(0,0,0);       /* set to black */
	    clear();
	    /* set back to current color */
	    RGBcolor(R_table[COLOR],G_table[COLOR],B_table[COLOR]);
#endif
	break ;
/*
 *************** set color index ****************************************
 */
    case 15:
	COLOR = BUFFER[0];
	RGBcolor(R_table[COLOR],G_table[COLOR],B_table[COLOR]);
	break ;
/*
 *************** flush buffer *******************************************
 */
    case 16:
	/* Empty */
	break ;
/*
 *************** read cursor ********************************************
 */
    case 17:
	xpos = BUFFER[0] ;
	ypos = BUFFER[1] ;
	STRING[0] = GR_cursor( &xpos, &ypos, "PGPLOT cursor input..." ) ;
	BUFFER[0] = xpos ;
	BUFFER[1] = ypos ;
	*NBUF = 2 ;
	*NSTR = 1 ;
	break ;
/*
 *************** erase alpha screen *************************************
 */
    case 18:
	/* no alpha screen to erase */
	break ;
/*
 *************** set line style *****************************************
 */
    case 19:
        setlinestyle((short) (BUFFER[0] - 1));
	break ;
/*
 *************** polygon fill *******************************************
 */
    case 20:
	if( POLY == 0 ) {
	    NPOLY = BUFFER[0] ;
	    POLY = NPOLY ;
	    }
	else {
	    VERTEX[--POLY][0] = (Icoord) BUFFER[0] ;
	    VERTEX[POLY][1] = (Icoord) BUFFER[1] ;
	    if( POLY == 0 )
		polf2i( NPOLY, VERTEX ) ;
	    }
	break ;
/*
 *************** set color represention *********************************
 */
    case 21:
	i = BUFFER[0] ;
	R_table[i] = 255.9 * BUFFER[1] ;
	G_table[i] = 255.9 * BUFFER[2] ;
	B_table[i] = 255.9 * BUFFER[3] ;
	break ;
/*
 *************** set line width *****************************************
 */
    case 22:
        linewidth((short)BUFFER[0]);
	break ;
/*
 *************** escape function ****************************************
 */
    case 23:
	/* no escape functions */
	break ;
/*
 *************** rectangle fill *****************************************
 */
    case 24:
	rectfi((Icoord)BUFFER[0],(Icoord)BUFFER[1],(Icoord)BUFFER[2],
               (Icoord)BUFFER[3]);
	break ;
/*
 *************** future unknown functions *******************************
 */
    default:
	*NBUF = -1 ;
	break ;
	}
    }

/************************************************************************
 *									*
 *	Suntools Dependent Garbage					*
 *									*
 ************************************************************************/

GR_start()
{

    /* Open the window we have defined */
    foreground();
    prefposition(150,150+X_WID,150,150+Y_WID);
    winopen("PGPLOT");

    /* Define our linestyles */
    deflinestyle(1, 0x3EEE); /* Long dash */
    deflinestyle(2, 0x6767); /* dot-dash-dot-dash */
    deflinestyle(3, 0x5555); /* dotted */
    deflinestyle(4, 0xFEEE); /* dash-dot-dot-dot */

    /* set RGB mode for color interpretation */
    RGBmode();

    /* call gconfig to reset system values to defaults */
    gconfig();

    /* clear screen */
    RGBcolor(0,0,0);
    clear();
    RGBcolor(255,255,255);

}
/************************************************************************
 *									*
 *	GR_cursor -- Wait for Events					*
 *									*
 ************************************************************************/

int x_cursor, y_cursor ;
unsigned char c_cursor ;

unsigned char GR_cursor( x, y, string )
int *x, *y ;
char *string ;
{
    /* definitions for use by queue commands */
    long dev;
    short val;

    x_cursor = -1 ;
    y_cursor = -1 ;
    c_cursor = 0xff ;

    /* queue the events we wish to look for */
    qdevice(KEYBD);
    qdevice(MOUSE3);

    /* print the string of instruction */
    printf("%s\n",string);

    /* test the input queue until we have an event */
    while( c_cursor == 0xff ) {
        if(qtest()) {
	   dev = qread(&val);
           switch(dev) {
              case KEYBD:
                 x_cursor = getvaluator(MOUSEX);
                 y_cursor = getvaluator(MOUSEY);
                 c_cursor = (unsigned char)val;
                 break;
              case MOUSE3:
                 x_cursor = getvaluator(MOUSEX);
                 y_cursor = getvaluator(MOUSEY);
                 c_cursor = 0x85;
                 break;
	      case REDRAW:
	         reshapeviewport();
	         break;
	      default:
	         break;
            }
         }
     }

    /* unqueue the events and reset the queue */
    unqdevice(KEYBD);
    unqdevice(MOUSE1);
    qreset();

    *x = x_cursor ;
    *y = y_cursor ;
    return c_cursor ;
}
