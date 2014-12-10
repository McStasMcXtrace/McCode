#import <appkit/appkit.h>
#import "pgvSpeaker.h"
#import <mach/mach.h>
#import <mach/message.h>
#import <servers/netname.h>
extern port_t name_server_port;
extern id NXResponsibleDelegate();
@implementation  pgvSpeaker :Speaker
{}
-(int)beginp
/* */
{
return [self selectorRPC:"beginp"
	paramTypes:""];
}
-(int)cursorat : (double *) xpos
	and : (double *) ypos
	char : (int *) ichar
/* */
{
return [self selectorRPC:"cursorat:and:char:"
	paramTypes:"DDI",
		xpos,
		ypos,
		ichar];
}
-(int)flush
/* */
{
return [self selectorRPC:"flush"
	paramTypes:""];
}
-(int)getwind : (int *) ixdim
	by : (int *) iydim
	scale : (double *) dmag
	color : (int *) icol
/* */
{
return [self selectorRPC:"getwind:by:scale:color:"
	paramTypes:"IIDI",
		ixdim,
		iydim,
		dmag,
		icol];
}
-(int)pscode : (char *) cbuf
/* */
{
return [self selectorRPC:"pscode:"
	paramTypes:"c",
		cbuf];
}
-(int)endp
/* */
{
return [self selectorRPC:"endp"
	paramTypes:""];
}
@end
