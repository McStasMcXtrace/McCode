#!/usr/bin/env python2.7
import sys, os


def usage():
    print 'usage: %s mcstas.sim' % sys.argv[0]


def main():
    # Check for --help flag
    if '--help' in sys.argv:
        usage()
        sys.exit(0)

    # Sanity check
    if len(sys.argv) < 2:
        usage()
        sys.exit(1)

    from mcdata import mcstas_to_plotdescs
    from plotting import PlotDesc, McLayout

    plotdatas = mcstas_to_plotdescs(sys.argv[1])
    McLayout(list(plotdatas)).configure_traits(scrollable=True)


if __name__ == "__main__":
    main()
