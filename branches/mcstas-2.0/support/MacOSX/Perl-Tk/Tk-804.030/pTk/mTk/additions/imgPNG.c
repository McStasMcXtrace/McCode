/*
 * tkImgPNG.c --
 *
 * A photo image file handler for PNG files.
 *
 * Uses the libpng.so library, which is dynamically
 * loaded only when used.
 *
 */

/* Author : Jan Nijtmans */
/* Date   : 2/13/97        */
/* Original implementation : Joel Crisp     */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "imgInt.h"

#undef EXTERN

#ifdef MAC_TCL
#include "libpng:png.h"
#else
#ifdef HAVE_IMG_H
#   include <png.h>
#else
#   include "libpng/png.h"
#endif
#endif

#ifdef __WIN32__
#define PNG_LIB_NAME "png.dll"
#endif

#ifndef PNG_LIB_NAME
#define PNG_LIB_NAME "libpng.so"
#endif

#define COMPRESS_THRESHOLD 1024

/*
 * The format record for the PNG file format:
 */


static int ChnMatchPNG _ANSI_ARGS_((Tcl_Channel chan, char *fileName,
	char *formatString, int *widthPtr, int *heightPtr));
static int FileMatchPNG _ANSI_ARGS_((FILE *f, char *fileName,
	char *formatString, int *widthPtr, int *heightPtr));
static int ObjMatchPNG _ANSI_ARGS_((struct Tcl_Obj *dataObj,
	char *formatString, int *widthPtr, int *heightPtr));
static int ChnReadPNG _ANSI_ARGS_((Tcl_Interp *interp, Tcl_Channel chan,
	char *fileName, char *formatString, Tk_PhotoHandle imageHandle,
	int destX, int destY, int width, int height, int srcX, int srcY));
static int FileReadPNG _ANSI_ARGS_((Tcl_Interp *interp, FILE *f,
	char *fileName, char *formatString, Tk_PhotoHandle imageHandle,
	int destX, int destY, int width, int height, int srcX, int srcY));
static int ObjReadPNG _ANSI_ARGS_((Tcl_Interp *interp, struct Tcl_Obj *dataObj,
	char *formatString, Tk_PhotoHandle imageHandle,
	int destX, int destY, int width, int height, int srcX, int srcY));
static int FileWritePNG _ANSI_ARGS_((Tcl_Interp *interp, char *filename,
	char *formatString, Tk_PhotoImageBlock *blockPtr));
static int StringWritePNG _ANSI_ARGS_((Tcl_Interp *interp,
	Tcl_DString *dataPtr, char *formatString,
	Tk_PhotoImageBlock *blockPtr));

Tk_PhotoImageFormat imgFmtPNG = {
    "PNG",					/* name */
    (Tk_ImageFileMatchProc *) ChnMatchPNG,	/* fileMatchProc */
    (Tk_ImageStringMatchProc *) ObjMatchPNG,	/* stringMatchProc */
    (Tk_ImageFileReadProc *) ChnReadPNG,	/* fileReadProc */
    (Tk_ImageStringReadProc *) ObjReadPNG,	/* stringReadProc */
    FileWritePNG,				/* fileWriteProc */
    (Tk_ImageStringWriteProc *) StringWritePNG,	/* stringWriteProc */
};

Tk_PhotoImageFormat imgOldFmtPNG = {
    "PNG",					/* name */
    (Tk_ImageFileMatchProc *) FileMatchPNG,	/* fileMatchProc */
    (Tk_ImageStringMatchProc *) ObjMatchPNG,/* stringMatchProc */
    (Tk_ImageFileReadProc *) FileReadPNG,	/* fileReadProc */
    (Tk_ImageStringReadProc *) ObjReadPNG,	/* stringReadProc */
    FileWritePNG,				/* fileWriteProc */
    (Tk_ImageStringWriteProc *) StringWritePNG,	/* stringWriteProc */
};

/*
 * Prototypes for local procedures defined in this file:
 */

static int CommonMatchPNG _ANSI_ARGS_((MFile *handle, int *widthPtr,
	int *heightPtr));
static int CommonReadPNG _ANSI_ARGS_((png_structp png_ptr, char *formatString,
	Tk_PhotoHandle imageHandle, int destX, int destY, int width,
	int height, int srcX, int srcY));
static int CommonWritePNG _ANSI_ARGS_((Tcl_Interp *interp, png_structp png_ptr,
	png_infop info_ptr, char *formatString,
	Tk_PhotoImageBlock *blockPtr));
static void tk_png_error _ANSI_ARGS_((png_structp, png_const_charp));
static void tk_png_warning _ANSI_ARGS_((png_structp, png_const_charp));
static int load_png_library _ANSI_ARGS_((Tcl_Interp *interp));

/*
 * these are for the BASE64 image reader code only
 */

static void	tk_png_memread _ANSI_ARGS_((png_structp, png_bytep,
		    png_size_t));
static void	tk_png_memwrite _ANSI_ARGS_((png_structp, png_bytep,
		    png_size_t));
static void	tk_png_chanread _ANSI_ARGS_((png_structp, png_bytep,
		    png_size_t));

static struct PngFunctions {
    VOID *handle;
    png_structp (* create_read_struct) _ANSI_ARGS_((png_const_charp,
	png_voidp, png_error_ptr, png_error_ptr));
    png_infop (* create_info_struct) _ANSI_ARGS_((png_structp));
    png_structp (* create_write_struct) _ANSI_ARGS_((png_const_charp,
	png_voidp, png_error_ptr, png_error_ptr));
    void (* destroy_read_struct) _ANSI_ARGS_((png_structpp,
	png_infopp, png_infopp));
    void (* destroy_write_struct) _ANSI_ARGS_((png_structpp, png_infopp));
    void (* error) _ANSI_ARGS_((png_structp, png_charp));
    png_byte (* get_channels) _ANSI_ARGS_((png_structp, png_infop));
    png_voidp (* get_error_ptr) _ANSI_ARGS_((png_structp));
    png_voidp (* get_progressive_ptr) _ANSI_ARGS_((png_structp));
    png_uint_32 (* get_rowbytes) _ANSI_ARGS_((png_structp, png_infop));
    png_uint_32 (* get_IHDR) _ANSI_ARGS_((png_structp, png_infop,
	   png_uint_32*, png_uint_32*, int*, int*, int*, int*, int*));
    void (* init_io) _ANSI_ARGS_((png_structp, FILE *));
    void (* read_image) _ANSI_ARGS_((png_structp, png_bytepp));
    void (* read_info) _ANSI_ARGS_((png_structp, png_infop));
    void (* read_update_info) _ANSI_ARGS_((png_structp, png_infop));
    int (* set_interlace_handling) _ANSI_ARGS_ ((png_structp));
    void (* set_read_fn) _ANSI_ARGS_((png_structp, png_voidp, png_rw_ptr));
    void (* set_text) _ANSI_ARGS_((png_structp, png_infop, png_textp, int));
    void (* set_write_fn) _ANSI_ARGS_((png_structp, png_voidp,
	    png_rw_ptr, png_voidp));
    void (* set_IHDR) _ANSI_ARGS_((png_structp, png_infop, png_uint_32,
	    png_uint_32, int, int, int, int, int));
    void (* write_end) _ANSI_ARGS_((png_structp, png_infop));
    void (* write_info) _ANSI_ARGS_((png_structp, png_infop));
    void (* write_row) _ANSI_ARGS_((png_structp, png_bytep));
    void (* set_expand) _ANSI_ARGS_((png_structp));
    void (* set_filler) _ANSI_ARGS_((png_structp, png_uint_32, int));
    void (* set_strip_16) _ANSI_ARGS_((png_structp));
} png = {0};

static char *symbols[] = {
    "png_create_read_struct",
    "png_create_info_struct",
    "png_create_write_struct",
    "png_destroy_read_struct",
    "png_destroy_write_struct",
    "png_error",
    "png_get_channels",
    "png_get_error_ptr",
    "png_get_progressive_ptr",
    "png_get_rowbytes",
    "png_get_IHDR",
    "png_init_io",
    "png_read_image",
    "png_read_info",
    "png_read_update_info",
    "png_set_interlace_handling",
    "png_set_read_fn",
    "png_set_text",
    "png_set_write_fn",
    "png_set_IHDR",
    "png_write_end",
    "png_write_info",
    "png_write_row",
    /* The following symbols are not crucial. All of them
       are checked at runtime. */
    "png_set_expand",
    "png_set_filler",
    "png_set_strip_16",
    (char *) NULL
};

typedef struct cleanup_info {
    Tcl_Interp *interp;
    char **data;
} cleanup_info;

static void
tk_png_error(png_ptr, error_msg)
    png_structp png_ptr;
    png_const_charp error_msg;
{
    cleanup_info *info;

    info = (cleanup_info *) png.get_error_ptr(png_ptr);
    if (info->data) {
	ckfree((char *) info->data);
    }
    Tcl_AppendResult(info->interp,
	    error_msg, (char *) NULL);
    longjmp(*(jmp_buf *) png_ptr,1);
}

static void
tk_png_warning(png_ptr, error_msg)
    png_structp png_ptr;
    png_const_charp error_msg;
{
    return;
}

static void
tk_png_memread(png_ptr, data, length)
    png_structp png_ptr;
    png_bytep data;
    png_size_t length;
{
    if (ImgRead((MFile *) png.get_progressive_ptr(png_ptr),
	    (char *) data, (size_t) length) != (int) length) {
	png.error(png_ptr, "Read Error");
    }
}

static void
tk_png_memwrite(png_ptr, data, length)
    png_structp png_ptr;
    png_bytep data;
    png_size_t length;
{
    if (ImgWrite((MFile *) png.get_progressive_ptr(png_ptr),
	    (char *) data, (size_t) length) != (int) length) {
	png.error(png_ptr, "Write Error");
    }
}

static void
tk_png_chanread(png_ptr, data, length)
    png_structp png_ptr;
    png_bytep data;
    png_size_t length;
{
    if (Tcl_Read((Tcl_Channel) png.get_progressive_ptr(png_ptr),
    	    (char *) data, (size_t) length) != (int) length) {
	png.error(png_ptr, "Read Error");
    }
}

static int ChnMatchPNG(chan, fileName, formatString, widthPtr, heightPtr)
    Tcl_Channel chan;
    char *fileName;
    char *formatString;
    int *widthPtr, *heightPtr;
{
    MFile handle;

    handle.data = (char *) chan;
    handle.state = IMG_CHAN;

    return CommonMatchPNG(&handle, widthPtr, heightPtr);
}

static int FileMatchPNG(f, fileName, formatString, widthPtr, heightPtr)
    FILE *f;
    char *fileName;
    char *formatString;
    int *widthPtr, *heightPtr;
{
    MFile handle;

    handle.data = (char *) f;
    handle.state = IMG_FILE;

    return CommonMatchPNG(&handle, widthPtr, heightPtr);
}

static int ObjMatchPNG(dataObj, formatString, widthPtr, heightPtr)
    struct Tcl_Obj *dataObj;
    char *formatString;
    int *widthPtr, *heightPtr;
{
    MFile handle;

    if (!ImgReadInit(dataObj,'\211',&handle)) {
	return 0;
    }
    return CommonMatchPNG(&handle, widthPtr, heightPtr);
}

static int CommonMatchPNG(handle, widthPtr, heightPtr)
    MFile *handle;
    int *widthPtr, *heightPtr;
{
    unsigned char buf[8];

    if ((ImgRead(handle, (char *) buf, 8) != 8)
	    || (strncmp("\211PNG\15\12\32\12", (char *) buf, 8) != 0)
	    || (ImgRead(handle, (char *) buf, 8) != 8)
	    || (strncmp("IHDR", (char *) buf+4, 4) != 0)
	    || (ImgRead(handle, (char *) buf, 8) != 8)) {
	return 0;
    }
    *widthPtr = (buf[0]<<24) + (buf[1]<<16) + (buf[2]<<8) + buf[3];
    *heightPtr = (buf[4]<<24) + (buf[5]<<16) + (buf[6]<<8) + buf[7];
    return 1;
}

static int
load_png_library(interp)
    Tcl_Interp *interp;
{
    if (ImgLoadLib(interp, PNG_LIB_NAME, &png.handle, symbols, 23)
	    != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

static int ChnReadPNG(interp, chan, fileName, formatString, imageHandle,
	destX, destY, width, height, srcX, srcY)
    Tcl_Interp *interp;
    Tcl_Channel chan;
    char *fileName;
    char *formatString;
    Tk_PhotoHandle imageHandle;
    int destX, destY;
    int width, height;
    int srcX, srcY;
{
    png_structp png_ptr;
    cleanup_info cleanup;

    if (load_png_library(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    cleanup.interp = interp;
    cleanup.data = NULL;

    png_ptr=png.create_read_struct(PNG_LIBPNG_VER_STRING,
	    (png_voidp) &cleanup,tk_png_error,tk_png_warning);
    if (!png_ptr) return(0);

    png.set_read_fn(png_ptr, (png_voidp) chan, tk_png_chanread);

    return CommonReadPNG(png_ptr, formatString, imageHandle, destX, destY,
	    width, height, srcX, srcY);
}

static int FileReadPNG(interp, f, fileName, formatString, imageHandle,
	destX, destY, width, height, srcX, srcY)
    Tcl_Interp *interp;
    FILE *f;
    char *fileName;
    char *formatString;
    Tk_PhotoHandle imageHandle;
    int destX, destY;
    int width, height;
    int srcX, srcY;
{
    png_structp png_ptr;
    cleanup_info cleanup;

    if (load_png_library(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    cleanup.interp = interp;
    cleanup.data = NULL;

    png_ptr=png.create_read_struct(PNG_LIBPNG_VER_STRING,
	    (png_voidp) &cleanup,tk_png_error,tk_png_warning);
    if (!png_ptr) return(0);

    png.init_io(png_ptr,f);

    return CommonReadPNG(png_ptr, formatString, imageHandle, destX, destY,
	    width,height,srcX,srcY);
}

static int ObjReadPNG(interp, dataObj, formatString, imageHandle,
	destX, destY, width, height, srcX, srcY)
    Tcl_Interp *interp;
    struct Tcl_Obj *dataObj;
    char *formatString;
    Tk_PhotoHandle imageHandle;
    int destX, destY;
    int width, height;
    int srcX, srcY;
{
    png_structp png_ptr;
    MFile handle;
    cleanup_info cleanup;

    if (load_png_library(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    cleanup.interp = interp;
    cleanup.data = NULL;

    png_ptr=png.create_read_struct(PNG_LIBPNG_VER_STRING,
	    (png_voidp) &cleanup,tk_png_error,tk_png_warning);
    if (!png_ptr) return TCL_ERROR;

    ImgReadInit(dataObj,'\211',&handle);

    png.set_read_fn(png_ptr,(png_voidp) &handle, tk_png_memread);

    return CommonReadPNG(png_ptr, formatString, imageHandle, destX, destY,
	    width, height, srcX, srcY);
}

typedef struct myblock {
    Tk_PhotoImageBlock ck;
    int dummy; /* extra space for offset[3], in case it is not
		  included already in Tk_PhotoImageBlock */
} myblock;

#define block bl.ck

static int CommonReadPNG(png_ptr, formatString, imageHandle, destX, destY,
	width, height, srcX, srcY)
    png_structp png_ptr;
    char *formatString;
    Tk_PhotoHandle imageHandle;
    int destX, destY;
    int width, height;
    int srcX, srcY;
{
    png_infop info_ptr;
    png_infop end_info;
    char **png_data = NULL;
    myblock bl;
    unsigned int I;
    png_uint_32 info_width, info_height;
    int bit_depth, color_type, interlace_type;

    info_ptr=png.create_info_struct(png_ptr);
    if (!info_ptr) {
	png.destroy_read_struct(&png_ptr,NULL,NULL);
	return(TCL_ERROR);
    }

    end_info=png.create_info_struct(png_ptr);
    if (!end_info) {
	png.destroy_read_struct(&png_ptr,&info_ptr,NULL);
	return(TCL_ERROR);
    }

    if (setjmp(*(jmp_buf *) png_ptr)) {
	png.destroy_read_struct(&png_ptr, &info_ptr, &end_info);
	return TCL_ERROR;
    }


    png.read_info(png_ptr,info_ptr);

    png.get_IHDR(png_ptr, info_ptr, &info_width, &info_height, &bit_depth,
	&color_type, &interlace_type, (int *) NULL, (int *) NULL);

    if ((srcX + width) > (int) info_width) {
	width = info_width - srcX;
    }
    if ((srcY + height) > (int) info_height) {
	height = info_height - srcY;
    }
    if ((width <= 0) || (height <= 0)
	|| (srcX >= (int) info_width)
	|| (srcY >= (int) info_height)) {
	return TCL_OK;
    }

    Tk_PhotoExpand(imageHandle, destX + width, destY + height);

    Tk_PhotoGetImage(imageHandle, &block);

    if (png.set_strip_16 != NULL) {
	png.set_strip_16(png_ptr);
    } else if (bit_depth == 16) {
	block.offset[1] = 2;
	block.offset[2] = 4;
    }

    if (png.set_expand != NULL) {
	png.set_expand(png_ptr);
    }

    png.read_update_info(png_ptr,info_ptr);
    block.pixelSize = png.get_channels(png_ptr, info_ptr);
    block.pitch = png.get_rowbytes(png_ptr, info_ptr);

    if ((color_type & PNG_COLOR_MASK_COLOR) == 0) {
	/* grayscale image */
	block.offset[1] = 0;
	block.offset[2] = 0;
    }
    block.width = width;
    block.height = height;

    if (color_type & PNG_COLOR_MASK_ALPHA) {
	/* with alpha channel */
	block.offset[3] = block.pixelSize - 1;
    } else {
	/* without alpha channel */
	block.offset[3] = 0;
    }

    png_data= (char **) ckalloc(sizeof(char *) * info_height +
	    info_height * block.pitch);

    ((cleanup_info *) png.get_error_ptr(png_ptr))->data = png_data;
    for(I=0;I<info_height;I++) {
	png_data[I]= ((char *) png_data) + (sizeof(char *) * info_height +
		I * block.pitch);
    }
    block.pixelPtr=(unsigned char *) (png_data[srcY]+srcX*block.pixelSize);

    png.read_image(png_ptr,(png_bytepp) png_data);

    ImgPhotoPutBlock(imageHandle,&block,destX,destY,width,height);

    ckfree((char *) png_data);
    ((cleanup_info *) png.get_error_ptr(png_ptr))->data = NULL;
    png_data=NULL;

    png.destroy_read_struct(&png_ptr,&info_ptr,&end_info);

    return(TCL_OK);
}

static int FileWritePNG(interp, filename, formatString, blockPtr)
    Tcl_Interp *interp;
    char *filename;
    char *formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    FILE *outfile = NULL;
    png_structp png_ptr;
    png_infop info_ptr;
    Tcl_DString nameBuffer;
    char *fullname;
    int result;
    cleanup_info cleanup;

    if ((fullname=Tcl_TranslateFileName(interp,filename,&nameBuffer))==NULL) {
	return TCL_ERROR;
    }

    if (!(outfile=fopen(fullname,"wb"))) {
	Tcl_AppendResult(interp, filename, ": ", Tcl_PosixError(interp),
		(char *)NULL);
	Tcl_DStringFree(&nameBuffer);
	return TCL_ERROR;
    }

    Tcl_DStringFree(&nameBuffer);

    if (load_png_library(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    cleanup.interp = interp;
    cleanup.data = (char **) NULL;

    png_ptr=png.create_write_struct(PNG_LIBPNG_VER_STRING,
	    (png_voidp) &cleanup,tk_png_error,tk_png_warning);
    if (!png_ptr) return TCL_ERROR;

    info_ptr=png.create_info_struct(png_ptr);
    if (!info_ptr) {
	png.destroy_write_struct(&png_ptr,NULL);
	fclose(outfile);
	return TCL_ERROR;
    }

    png.init_io(png_ptr,outfile);

    result = CommonWritePNG(interp, png_ptr, info_ptr, formatString, blockPtr);
    fclose(outfile);
    return result;
}

static int StringWritePNG(interp, dataPtr, formatString, blockPtr)
    Tcl_Interp *interp;
    Tcl_DString *dataPtr;
    char *formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    png_structp png_ptr;
    png_infop info_ptr;
    MFile handle;
    int result;
    cleanup_info cleanup;

    if (load_png_library(interp) != TCL_OK) {
	return TCL_ERROR;
    }

    cleanup.interp = interp;
    cleanup.data = (char **) NULL;

    png_ptr=png.create_write_struct(PNG_LIBPNG_VER_STRING,
	    (png_voidp) &cleanup,tk_png_error,tk_png_warning);
    if (!png_ptr) return TCL_ERROR;

    info_ptr = png.create_info_struct(png_ptr);
    if (!info_ptr) {
	png.destroy_write_struct(&png_ptr,NULL);
	return TCL_ERROR;
    }

    png.set_write_fn(png_ptr,(png_voidp) &handle, tk_png_memwrite, (png_voidp) NULL);

    ImgWriteInit(dataPtr, &handle);

    result = CommonWritePNG(interp, png_ptr, info_ptr, formatString, blockPtr);
    ImgPutc(IMG_DONE, &handle);
    return result;
}

static int CommonWritePNG(interp, png_ptr, info_ptr, formatString, blockPtr)
    Tcl_Interp *interp;
    png_structp png_ptr;
    png_infop info_ptr;
    char *formatString;
    Tk_PhotoImageBlock *blockPtr;
{
    int greenOffset, blueOffset, alphaOffset;
    int tagcount = 0;
    char **tags = NULL;
    int I, pass, number_passes, color_type;
    int newPixelSize;
    png_bytep row_pointers;
    png_textp text = NULL;

    if (formatString != NULL) {
	if (Tcl_SplitList(interp,formatString,&tagcount,&tags)!=TCL_OK) {
	    Tcl_AppendResult(interp,"invalid format: \"",
		    formatString, "\"",(char *) NULL);
	    return TCL_ERROR;
	}
	tagcount = tagcount/2 - 1;
	if (tagcount < 0) {tagcount = 0;}
    }

    if (setjmp(*(jmp_buf *)png_ptr)) {
	if (tags) {
	    ckfree((char *) tags);
	}
	if (text) {
	    ckfree((char *) text);
	}
	png.destroy_write_struct(&png_ptr,&info_ptr);
	return TCL_ERROR;
    }
    greenOffset = blockPtr->offset[1] - blockPtr->offset[0];
    blueOffset = blockPtr->offset[2] - blockPtr->offset[0];
    alphaOffset = blockPtr->offset[0];
    if (alphaOffset < blockPtr->offset[2]) {
	alphaOffset = blockPtr->offset[2];
    }
    if (++alphaOffset < blockPtr->pixelSize) {
	alphaOffset -= blockPtr->offset[0];
    } else {
	alphaOffset = 0;
    }

    if (greenOffset || blueOffset) {
	color_type = PNG_COLOR_TYPE_RGB;
	newPixelSize = 3;
    } else {
	color_type = PNG_COLOR_TYPE_GRAY;
	newPixelSize = 1;
    }
    if (alphaOffset) {
	color_type |= PNG_COLOR_MASK_ALPHA;
	newPixelSize++;
#if 0 /* The function png_set_filler doesn't seem to work; don't known why :-( */
    } else if ((blockPtr->pixelSize==4) && (newPixelSize == 3)
	    && (png.set_filler != NULL)) {
	/*
	 * The set_filler() function doesn't need to be called
	 * because the code below can handle all necessary
	 * re-allocation of memory. Only it is more economically
	 * to let the PNG library do that, which is only
	 * possible with v0.95 and higher.
	 */
	png.set_filler(png_ptr, 0, PNG_FILLER_AFTER);
	newPixelSize++;
#endif
    }

    png.set_IHDR(png_ptr, info_ptr, blockPtr->width, blockPtr->height, 8,
	    color_type, PNG_INTERLACE_ADAM7, PNG_COMPRESSION_TYPE_BASE,
	    PNG_FILTER_TYPE_BASE);

    if (tagcount > 0) {
	png_text text;
	for(I=0;I<tagcount;I++) {
	    text.compression = 0;
	    text.key = tags[2*I+1];
	    text.text = tags[2*I+2];
	    text.text_length=strlen(tags[2*I+2]);
	    if (text.text_length>COMPRESS_THRESHOLD) {
		text.compression = -1;
	    }
	    png.set_text(png_ptr, info_ptr, &text, 1);
        }
    }
    png.write_info(png_ptr,info_ptr);

    number_passes = png.set_interlace_handling(png_ptr);

    if (blockPtr->pixelSize != newPixelSize) {
	int J, oldPixelSize;
	png_bytep src, dst;
	oldPixelSize = blockPtr->pixelSize;
	row_pointers = (png_bytep)
		ckalloc(blockPtr->width * newPixelSize);
	for (pass = 0; pass < number_passes; pass++) {
	    for(I=0;I<blockPtr->height;I++) {
		src = (png_bytep) blockPtr->pixelPtr
			+ I * blockPtr->pitch + blockPtr->offset[0];
		dst = row_pointers;
		for (J = blockPtr->width; J > 0; J--) {
		    memcpy(dst, src, newPixelSize);
		    src += oldPixelSize;
		    dst += newPixelSize;
		}
		png.write_row(png_ptr, row_pointers);
	    }
	}
	ckfree((char *) row_pointers);
    } else {
	for (pass = 0; pass < number_passes; pass++) {
	    for(I=0;I<blockPtr->height;I++) {
		row_pointers = (png_bytep) blockPtr->pixelPtr
			+ I * blockPtr->pitch + blockPtr->offset[0];
		png.write_row(png_ptr, row_pointers);
	    }
	}
    }
    png.write_end(png_ptr,NULL);
    if (text) {
	ckfree((char *) text);
    }
    if (tags) {
	ckfree((char *) tags);
    }
    png.destroy_write_struct(&png_ptr,&info_ptr);

    return(TCL_OK);
}
