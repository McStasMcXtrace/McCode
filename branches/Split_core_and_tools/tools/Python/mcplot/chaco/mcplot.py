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


def plot_desc(desc):
    plot = Plot(desc.data)
    plot.plot((desc.x, desc.y), type=desc.type, title=desc.title, color=desc.color)
    return plot


class McPlot(HasTraits):
    plot = Instance(PCont)
    reset = Button('Reset')

    traits_view = View(
        Item('reset', show_label=False),
        Item('plot',editor=ComponentEditor(), show_label=False),
        width=500, height=500, resizable=True, title="Chaco Plot")

    def __init__(self, descs):
        super(McPlot, self).__init__()

        plots = map(plot_desc, descs)

        w = int(ceil(sqrt(len(plots))))
        h = int(ceil(len(plots) / float(w)))

        empty = Plot()
        plots = plots + ((len(plots) - (w * h)) * [empty])

        self.plot = PCont(shape=(w, h))

        # add zoom and pan
        self.pans  = []
        self.zooms = []
        for p in plots:
            self.plot.add(p)

            if p is empty:
                continue

            pan = MTool(p)
            p.tools.append(pan)
            self.pans.append(pan)

            zoom = ZTool(p, tool_mode='box', always_on=True, drag_button='right')
            p.overlays.append(zoom)
            self.zooms.append(zoom)

            save = SaveTool(self.plot, always_on=True, filename="plot.pdf")
            p.tools.append(save)


    def _reset_changed(self):
        map(lambda z: z._reset_state_pressed(), self.zooms)

if __name__ == "__main__":
    x = linspace(-14, 14, 1000)
    y = sin(x) * x**3
    plotdata = ArrayPlotData(x=x, y=y)

    desc1 = PlotDesc('x', 'y', title='plot1', data=plotdata)
    desc2 = PlotDesc('y', 'x', title='plot2', data=plotdata)
    desc3 = PlotDesc('x', 'y', title='plot3', data=plotdata, type='scatter')
    desc4 = PlotDesc('y', 'x', title='plot4', data=plotdata, type='scatter')

    McPlot([desc1, desc2, desc3, desc4, desc4]).configure_traits()
