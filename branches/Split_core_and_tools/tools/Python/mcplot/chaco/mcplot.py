from traits.api import HasTraits, Instance, Button
from traitsui.api import View, Item

from chaco.api import Plot, GridPlotContainer as PCont, ArrayPlotData
from chaco.tools.api import PanTool as MTool, BetterSelectingZoom as ZTool, SaveTool

from enable.component_editor import ComponentEditor
from numpy import linspace, sin

from math import ceil, sqrt


class PlotDesc(object):
    def __init__(self, x, y, data, type='line', title=None, color='blue'):
        self.x = x
        self.y = y
        self.data = data
        self.type = type
        self.title = title
        self.color = color


class McPlot(Plot):
    def __init__(self, *args, **kwargs):
        super(McPlot, self).__init__(*args, **kwargs)

emptyPlot = McPlot()


class McLayout(HasTraits):
    layout = Instance(PCont)
    reset = Button('Reset')

    traits_view = View(
        Item('reset', show_label=False),
        Item('layout',editor=ComponentEditor(), show_label=False),
        width=500, height=500, resizable=True, title="Chaco Plot")

    def __init__(self, descs, focus=None):
        super(McLayout, self).__init__()
        self.descs = descs
        self.pans = []
        self.zooms = []
        self.plots = []
        self.focus = descs if focus is None else focus
        self.reinit()

    def reinit(self):
        self.pans  = []
        self.zooms = []

        self.plots = map(self.plot_desc, self.focus)
        num = len(self.plots)

        w = int(ceil(sqrt(num)))
        h = int(ceil(num / float(w)))

        self.plots = self.plots + ((num - w * h) * [emptyPlot])

        self.layout = PCont(shape=(w, h))

        # add zoom and pan
        for p in self.plots:
            self.layout.add(p)


    def plot_desc(self, desc):
        plot = McPlot(desc.data)
        plot.plot((desc.x, desc.y), type=desc.type, title=desc.title, color=desc.color)

        pan = MTool(plot)
        plot.tools.append(pan)
        self.pans.append(pan)

        zoom = ZTool(plot, tool_mode='box', always_on=True, drag_button='right')
        plot.overlays.append(zoom)
        self.zooms.append(zoom)

        save = SaveTool(self.layout, always_on=True, filename="plot.pdf")
        plot.tools.append(save)

        return plot


    def _reset_changed(self):
        self.focus = self.descs
        self.reinit()


def main():
    x = linspace(-14, 14, 1000)
    y = sin(x) * x**3
    plotdata = ArrayPlotData(x=x, y=y)

    desc1 = PlotDesc('x', 'y', title='plot1', data=plotdata)
    desc2 = PlotDesc('y', 'x', title='plot2', data=plotdata)
    desc3 = PlotDesc('x', 'y', title='plot3', data=plotdata, type='scatter')
    desc4 = PlotDesc('y', 'x', title='plot4', data=plotdata, type='scatter')

    McLayout([desc1, desc2, desc3, desc4, desc4]).configure_traits(scrollable=True)


if __name__ == "__main__":
    main()
