from mcdata import monitor_to_plotdata
from plotting import PlotDesc, McLayout


def main():
    plotdata = monitor_to_plotdata(file('DC_OUT_T_1342100227.t'))

    desc1 = PlotDesc('t', 'I', title='plot1', data=plotdata, value_scale='log', type='line')
    McLayout([desc1]).configure_traits(scrollable=True)



if __name__ == "__main__":
    main()
