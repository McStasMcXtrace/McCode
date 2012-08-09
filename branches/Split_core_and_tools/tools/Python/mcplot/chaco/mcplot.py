from traits.api import HasTraits, Instance, Button
from traitsui.api import View, Item

from chaco.api import Plot, GridPlotContainer as PCont, ArrayPlotData
from chaco.tools.api import PanTool as MTool, BetterSelectingZoom as ZTool, SaveTool

from enable.component_editor import ComponentEditor
from numpy import linspace, sin

class LinePlot(HasTraits):
    plot = Instance(PCont)
    reset = Button('Reset')

    traits_view = View(
        Item('reset', show_label=False),
        Item('plot',editor=ComponentEditor(), show_label=False),
        width=500, height=500, resizable=True, title="Chaco Plot")

    def __init__(self):
        super(LinePlot, self).__init__()

        x = linspace(-14, 14, 1000)
        y = sin(x) * x**3

        plotdata = ArrayPlotData(x=x, y=y)

        line = Plot(plotdata)
        line.plot(("x", "y"), type="line", color="blue")
        line.title = "sin(x) * x^3"

        line2 = Plot(plotdata)
        line2.plot(("y", "x"), type="line", color="blue")
        line2.title = "sin(x) * x^3"

        scatter = Plot(plotdata)
        scatter.plot(("x", "y"), type="scatter", color="blue")
        scatter.title = "sin(x) * x^3"

        scatter2 = Plot(plotdata)
        scatter2.plot(("y", "x"), type="scatter", color="blue")
        scatter2.title = "sin(x) * x^3"

        self.plot = PCont(shape=(2, 2))

        # add zoom and pan
        self.pans  = []
        self.zooms = []
        for p in (line, line2, scatter, scatter2):
            self.plot.add(p)

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
    LinePlot().configure_traits()
