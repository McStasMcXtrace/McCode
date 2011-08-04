/************************************************************************
 *
 *  svdriv -- SunView PGPLOT Driver
 *
 *  Brian M Sutin, May 19, 1989   -- first version
 *  Brian M Sutin, July 25, 1989  -- added arbitrary sized windows
 *  Jim Morgan, Jan 8, 1991       -- arbitrary separator in size.
 *  Sam Southard, Apr 30, 1991    -- Modified for sharable libraries.
 *  Tim Pearson, Jan 23, 1991     -- Remove prompt on exit.
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
 *	"/sunview"		--> 7.8" x 7.8" window (640x640)
 *	"X/sunview"		--> X" x X" window
 *	"X,Y/sunview"		--> X" x Y" window
 *
 *	X and Y are floating or integers, and have intelligent defaults.
 *      The comma can be replaced by any non-numeric character.
 *
 */

/*
 *  dev_name -- the PGPLOT device name
 *
 */
extern char sv_dev_name[] ;
/*
 *  SCALE -- the number of pixels per inch
 *
 *  This value is only good for typical Sun workstations, and will
 *  result in a teeny window on high resolution workstations.  Suns
 *  are also slightly non-square, resulting in the X and Y scales
 *  each being off from 82 by about 1 pixel in opposite directions.
 *
 */
#define SCALE		82
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
#define MAX_X_WID	1152
#define MAX_Y_WID	900
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
/*
 *  FLIPY -- flip Y coordinate
 *
 */
#define FLIPY(x)	( Y_WID - 1 - (x) )

extern int sv_COLOR ;		/* current pen color */
extern int sv_APPEND ;			/* to erase or not */

/*
 *  NCOLOR -- number of colors
 *
 *	PGPLOT predefines 16 colors, and suntools uses the last color
 *	for the cursor color, so the next factor of two (32) was used.
 *	Using a large number such as 256 will crowd the Sun color lookup
 *	table and cause strange behaviour with multiple windows.
 *
 */

#define NCOLOR	32

/*  The pre-defined dark grey color is too dark */

extern unsigned char sv_R_table[NCOLOR] ;
extern unsigned char sv_G_table[NCOLOR] ;
extern unsigned char sv_B_table[NCOLOR] ;

/*
 *  These variables are used for the Suntools polygon fill routine
 *
 */
extern int sv_NPOLY , sv_POLY ;
static int VERTEX[2048] ;

unsigned char GR_cursor() ;		/* get character at cursor */

void svdriv_( FUNC, BUFFER, NBUF, STRING, NSTR, len )
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
	strcpy( STRING, sv_dev_name ) ;
	*NSTR = strlen( sv_dev_name ) ;
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
	STRING[2] = 'N' ;		/* no dashed lines */
	STRING[3] = 'A' ;		/* polygon fill available */
	STRING[4] = 'N' ;	/* T */	/* fat lines */
	STRING[5] = 'R' ;		/* rectangle fill available */
	STRING[6] = 'N' ;		/* not used */
	STRING[7] = 'V' ;		/* window lost on exit */
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
	    sv_APPEND = 1 ;
	else
	    sv_APPEND = 0 ;
	*NBUF = 2 ;
/*	i = sscanf( STRING, "%f,%f", &REQ_X_WID, &REQ_Y_WID ) ; */
	i = sscanf( STRING, "%f%*c%f", &REQ_X_WID, &REQ_Y_WID ) ;
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
	GR_batch(1) ;
	break ;
/*
 *************** close workstation **************************************
 */
    case 10:
	GR_batch(0) ;
	break ;
/*
 *************** begin picture ******************************************
 */
    case 11:
	GR_batch(1) ;
	if( !sv_APPEND )			/* erase screen */
	    GR_rectangle( 0, 0, X_WID-1, Y_WID-1, 0 ) ;
	break ;
/*
 *************** draw line **********************************************
 */
    case 12:
	GR_vector( (int) BUFFER[0], (int) BUFFER[1],
		   (int) BUFFER[2], (int) BUFFER[3], sv_COLOR ) ;
	break ;
/*
 *************** draw dot ***********************************************
 */
    case 13:
	GR_vector( (int) BUFFER[0], (int) BUFFER[1],
		   (int) BUFFER[0], (int) BUFFER[1], sv_COLOR ) ;
	break ;
/*
 *************** end picture ********************************************
 */
    case 14:
#ifdef CORRECT_END
	if( BUFFER[0] )
	    GR_rectangle( 0, 0, X_WID-1, Y_WID-1, 0 ) ;
#endif CORRECT_END
	GR_batch(0) ;
	break ;
/*
 *************** set color index ****************************************
 */
    case 15:
	sv_COLOR = BUFFER[0] ;
	break ;
/*
 *************** flush buffer *******************************************
 */
    case 16:
	GR_batch(0) ;
	GR_batch(1) ;
	break ;
/*
 *************** read cursor ********************************************
 */
    case 17:
	GR_batch(0) ;
	xpos = BUFFER[0] ;
	ypos = FLIPY( BUFFER[1] ) ;
	STRING[0] = GR_cursor( &xpos, &ypos, "PGPLOT cursor input..." ) ;
	BUFFER[0] = xpos ;
	BUFFER[1] = FLIPY( ypos ) ;
	*NBUF = 2 ;
	*NSTR = 1 ;
	GR_batch(1) ;
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
	*NBUF = -1 ;			/* let pgplot do it */
	break ;
/*
 *************** polygon fill *******************************************
 */
    case 20:
	if( sv_POLY == 0 ) {
	    sv_NPOLY = BUFFER[0] ;
	    sv_POLY = 2 * sv_NPOLY ;
	    }
	else {
	    VERTEX[--sv_POLY] = FLIPY( BUFFER[1] ) ;
	    VERTEX[--sv_POLY] = BUFFER[0] ;
	    if( sv_POLY == 0 )
		GR_poly( sv_NPOLY, VERTEX, sv_COLOR ) ;
	    }
	break ;
/*
 *************** set color represention *********************************
 */
    case 21:
	i = BUFFER[0] ;
	sv_R_table[i] = 255.9 * BUFFER[1] ;
	sv_G_table[i] = 255.9 * BUFFER[2] ;
	sv_B_table[i] = 255.9 * BUFFER[3] ;
	GR_color(i) ;
	break ;
/*
 *************** set line width *****************************************
 */
    case 22:
	*NBUF = -1 ;				/* no fat lines */
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
	GR_rectangle( (int) BUFFER[0], (int) BUFFER[1],
		      (int) BUFFER[2], (int) BUFFER[3], sv_COLOR ) ;
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

#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <sys/time.h>
#include <signal.h>

Frame frame ;
Canvas canvas ;
Pixwin *pw ;

GR_start()
{
    static struct itimerval tvalue = {
	{ 0, 100000 },			/* 0.1 second interval */
	{ 0, 100000 }			/* 0.1 second value */
	} ;
    static update() ;			/* update screen */
    static gr_events() ;		/* interrupt handler */
    Notify_value gr_quit() ;		/* quit handler */

    frame = window_create(NULL, FRAME,
	FRAME_LABEL,			"PGPLOT",
	WIN_ERROR_MSG,			"Cannot create window",
	FRAME_SUBWINDOWS_ADJUSTABLE,	FALSE,
	0) ;

    canvas = window_create(frame, CANVAS,
	CANVAS_RETAINED,	FALSE,
	WIN_WIDTH,		X_WID,
	WIN_HEIGHT,		Y_WID,
	WIN_EVENT_PROC,		gr_events,
	0) ;

    pw = (Pixwin *) window_get(canvas, CANVAS_PIXWIN) ;
    pw_setcmsname( pw, "PGPLOT" ) ;
    pw_putcolormap( pw, 0, NCOLOR, sv_R_table, sv_G_table, sv_B_table ) ;

    window_set( canvas, CANVAS_RETAINED, TRUE, 0 ) ;
    window_fit(frame) ;
    window_set( frame, WIN_SHOW, TRUE, 0 ) ;
    (void) notify_interpose_destroy_func( frame, gr_quit ) ;
    (void) notify_dispatch() ;			/* set up signal catching */
    signal( SIGALRM, update ) ;			/* intercept alarm signals */
    setitimer( 0, &tvalue, 0 ) ;		/* set up our own timer */
    }

Notify_value gr_quit( client, status )
Notify_client client ;
Destroy_status status ;
{
    if( status != DESTROY_CHECKING )
	exit(1) ;
    return notify_next_destroy_func( client, status ) ;
    }
    
static update( sig, code, scp )
int sig, code ;
struct sigcontext *scp ;
{
    (void) notify_dispatch() ;			/* update the screen */
    }

/************************************************************************
 *									*
 *	Graphics Routines						*
 *									*
 ************************************************************************/

GR_rectangle( x_lo, y_lo, x_hi, y_hi, color )
int x_lo, y_lo, x_hi, y_hi, color ;
{
    pw_rop( pw, x_lo, FLIPY(y_hi), x_hi - x_lo, y_hi - y_lo,
	PIX_SRC | PIX_COLOR(color), 0, 0, 0 ) ;
    }
 
GR_vector( x1, y1, x2, y2, color )
int x1, y1, x2, y2, color ;
{
    pw_vector( pw, x1, FLIPY(y1), x2, FLIPY(y2),
	PIX_SRC | PIX_COLOR(color), 0 ) ;
    }

GR_poly( npoly, vertex, color )
int npoly, color ;
int *vertex ;
{
    int i ;

/*  pw_batch_on(pw) ; */
    pw_polygon_2( pw, 0, 0, 1, &npoly, vertex,
	PIX_SRC | PIX_COLOR(color), 0, 0, 0 ) ;

    for( i = 0 ; i < npoly-1 ; i++ )
	pw_vector( pw, vertex[2*i], vertex[2*i+1],
	    vertex[2*i+2], vertex[2*i+3],
	    PIX_SRC | PIX_COLOR(color), 0 ) ;
    pw_vector( pw, vertex[2*i], vertex[2*i+1],
	vertex[0], vertex[1],
	PIX_SRC | PIX_COLOR(color), 0 ) ;
/*  pw_batch_off(pw) ; */
    }

GR_color(c)
int c ;
{
    pw_putcolormap( pw, c, 1, &sv_R_table[c], &sv_G_table[c], &sv_B_table[c] ) ;
    }

GR_batch(state)
int state ;
{
    static int STATE = 0 ;

    if( state == STATE )
	return ;

    if( state )
	pw_batch_on(pw) ;
    else
	pw_batch_off(pw) ;

    STATE = state ;
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
    static struct timeval tvalue = {
	0, 100000,			/* 0.1 second interval */
	} ;
    x_cursor = -1 ;
    y_cursor = -1 ;
    c_cursor = 0xff ;

    window_set( canvas,
	WIN_CONSUME_KBD_EVENTS,		/* enable events */
	    WIN_ASCII_EVENTS,
	    WIN_LEFT_KEYS,		/* KEYS are here to remove    */
	    WIN_RIGHT_KEYS,		/* escape sequence definition */
	    WIN_TOP_KEYS,
	    0,
	WIN_MOUSE_XY,			/* must be after KEYS, or will */
	    *x, *y,			/* not highlight border correctly */
	0 ) ;
    window_set( frame, FRAME_LABEL, string, 0 ) ;

    while( c_cursor == 0xff )
	select( 0, 0, 0, 0, &tvalue ) ;	/* wait until character */

    window_set( canvas,			/* disable events */
	WIN_IGNORE_KBD_EVENTS,
	    WIN_ASCII_EVENTS,
	    WIN_LEFT_KEYS,		/* KEYS are here to remove    */
	    WIN_RIGHT_KEYS,		/* escape sequence definition */
	    WIN_TOP_KEYS,
	    0,
	0 ) ;
    window_set( frame, FRAME_LABEL, "PGPLOT", 0 ) ;

    *x = x_cursor ;
    *y = y_cursor ;
    return c_cursor ;
    }

/************************************************************************
 *									*
 *	gr_events -- Interrupt Handler					*
 *									*
 ************************************************************************/

static gr_events( win, event, arg )
Canvas win ;
Event *event ;
caddr_t arg ;
{
    int id ;
    float w_save, h_save ;

    id = event_id(event) ;
    x_cursor = event_x(event) ;			/* position */
    y_cursor = event_y(event) ;

    if( event_is_ascii(event) )			/* keyboard */
	c_cursor = id ;
    else
    if( event_is_button(event) ) {		/* mouse keys */
	c_cursor = 128 + 2 * ( id - BUT(1) ) ;
	if( event_is_up(event) )
	    c_cursor++ ;
	}
    else					/* everything else */
	return ;
    }
