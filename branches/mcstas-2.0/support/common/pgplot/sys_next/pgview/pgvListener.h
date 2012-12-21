#import <appkit/Listener.h>
@interface pgvListener : Listener
{}
-(int)beginp;
-(int)cursorat : (double *) xpos
	and : (double *) ypos
	char : (int *) ichar;
-(int)flush;
-(int)getwind : (int *) ixdim
	by : (int *) iydim
	scale : (double *) dmag
	color : (int *) icol;
-(int)pscode : (char *) cbuf;
-(int)endp;
@end
