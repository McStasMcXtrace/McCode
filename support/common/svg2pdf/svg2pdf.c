/* gcc `pkg-config --cflags --libs librsvg-2.0 cairo-pdf` -o svg2pdf svg2pdf.c
 *
 * Copyright © 2005 Red Hat, Inc.
 * Copyright © 2006 Red Hat, Inc.
 * Copyright © 2007 Red Hat, Inc.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Authors: Kristian Høgsberg <krh@redhat.com>
 *	    Carl Worth <cworth@redhat.com>
 *	    Behdad Esfahbod <besfahbo@redhat.com>
 */

#include <stdio.h>
#include <stdlib.h>
#include <librsvg/rsvg.h>
#include <librsvg/rsvg-cairo.h>

#include <cairo-pdf.h>

#define FAIL(msg)							\
    do { fprintf (stderr, "FAIL: %s\n", msg); exit (-1); } while (0)

#define PIXELS_PER_POINT 1

int main (int argc, char *argv[])
{
    GError *error = NULL;
    RsvgHandle *handle;
    RsvgDimensionData dim;
    double width, height;
    const char *filename = argv[1];
    const char *output_filename = argv[2];
    cairo_surface_t *surface;
    cairo_t *cr;
    cairo_status_t status;

    if (argc != 3)
	FAIL ("usage: svg2pdf input_file.svg output_file.pdf");

    g_type_init ();

    rsvg_set_default_dpi (72.0);
    handle = rsvg_handle_new_from_file (filename, &error);
    if (error != NULL)
	FAIL (error->message);

    rsvg_handle_get_dimensions (handle, &dim);
    width = dim.width;
    height = dim.height;

    surface = cairo_pdf_surface_create (output_filename, width, height);
    cr = cairo_create (surface);

    rsvg_handle_render_cairo (handle, cr);

    status = cairo_status (cr);
    if (status)
	FAIL (cairo_status_to_string (status));

    cairo_destroy (cr);
    cairo_surface_destroy (surface);

    return 0;
}
