from mcdata import mcstas_to_plotdescs
from plotting import PlotDesc, McLayout


def main():
    plotdatas = mcstas_to_plotdescs('mcstas')
    McLayout(list(plotdatas)).configure_traits(scrollable=True)


if __name__ == "__main__":
    main()
