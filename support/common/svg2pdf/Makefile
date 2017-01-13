ALL=svg2pdf

MYCFLAGS=`pkg-config --cflags librsvg-2.0 cairo-pdf` -Wall -Wpointer-arith -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wnested-externs -fno-strict-aliasing
MYLDFLAGS=`pkg-config --libs librsvg-2.0 cairo-pdf`

all: $(ALL)

%.o: %.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(MYCFLAGS) $< -o $@

%: %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(MYCFLAGS) $(MYLDFLAGS) $^ -o $@

clean:
	rm -f $(ALL) *.o
