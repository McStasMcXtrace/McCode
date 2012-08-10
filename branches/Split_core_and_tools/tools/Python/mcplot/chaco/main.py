import sys


def main():
    if len(sys.argv) < 2:
        print 'usage: %s mcstas.sim' % sys.argv[0]
        sys.exit(1)

    from mcdata import mcstas_to_plotdescs
    from plotting import PlotDesc, McLayout

    plotdatas = mcstas_to_plotdescs(sys.argv[1])
    McLayout(list(plotdatas)).configure_traits(scrollable=True)


if __name__ == "__main__":
    main()
