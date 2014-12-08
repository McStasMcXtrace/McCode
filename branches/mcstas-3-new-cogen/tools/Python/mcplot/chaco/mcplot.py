#!/usr/bin/env python2.7
import sys, os


def usage():
    print 'usage: %s [data dir|monitor datafile]' % os.path.basename(sys.argv[0])


def main():
    # Check for --help flag
    if '--help' in sys.argv:
        usage()
        sys.exit(0)

    # Sanity check
    if len(sys.argv) < 2:
        usage()
        sys.exit(1)

    simfile = os.path.abspath(sys.argv[1])
    if os.path.isdir(simfile):
        simfile = os.path.join(simfile, 'mccode.sim')


    from mcdata import mcstas_to_plotdescs
    from plotting import PlotDesc, McLayout

    # Use mcstas.sim position as window title
    titlepath = os.path.abspath(simfile)

    # Remove mcstas.sim (its implied)
    if titlepath.endswith('/mccode.sim'):
        titlepath = os.path.dirname(titlepath) + '/'

    # Truncate from front of too large
    if len(titlepath) > 50:
        titlepath = '...%s' % titlepath[-47:]

    # Construct final title
    title = 'mcplot: %s' % titlepath

    # Load data
    plotdatas = mcstas_to_plotdescs(simfile)

    # Create plots
    McLayout(list(plotdatas), window_title=title).configure_traits(
        scrollable=True)


if __name__ == "__main__":
    main()
