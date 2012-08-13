# traits
from traits.api import HasTraits, Instance, Button

# ui
from traitsui.view import View
from traitsui.item import Item

# general
from chaco.plot import Plot
from chaco.errorbar_plot import ErrorBarPlot
from chaco.plot_containers import GridPlotContainer as PCont
from chaco.array_data_source import ArrayDataSource
from chaco.default_colormaps import jet

# tools
from chaco.tools.pan_tool import PanTool as MTool
from chaco.tools.better_selecting_zoom import BetterSelectingZoom as ZTool
from chaco.tools.save_tool import SaveTool

# enable
from enable.component_editor import ComponentEditor
from enable.base_tool import BaseTool

# other
from numpy import linspace, sin
from math import ceil, sqrt


class FocusTool(BaseTool):
    def __init__(self, mclayout, desc):
        super(FocusTool, self).__init__()
        self.mclayout = mclayout
        self.desc = desc

    def normal_left_dclick(self, *args):
        if len(self.mclayout.focus) == 1:
            # go back to overview
            self.mclayout.focus = self.mclayout.descs
        else:
            # focus on chosen plot
            self.mclayout.focus = [self.desc]
        # reinit layout
        self.mclayout.reinit()


class PlotDesc(object):
    def __init__(self, x, y, data, **kwargs):
        self.x = x
        self.y = y
        self.data = data
        self.params = dict(type='line', color='blue')
        self.params.update(kwargs)


class McPlot(Plot):
    def __init__(self, *args, **kwargs):
        super(McPlot, self).__init__(*args, **kwargs)


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
        # clean up plots
        for p in self.plots:
            map(p.remove_trait, p.__dict__.keys())

        self.pans  = []
        self.zooms = []

        num = len(self.focus)
        w = int(ceil(sqrt(num)))
        h = int(ceil(num / float(w)))

        self.layout = PCont(shape=(w, h))

        self.plots = map(self.plot_desc, self.focus)
        map(self.layout.add, self.plots)


    def plot_desc(self, desc):
        plot = McPlot(desc.data)

        if None not in (desc.data['value_low'], desc.data['value_high']):
            # 1d
            plot.plot((desc.x, desc.y), name='data', **desc.params)

            # extract and prepare data for error bars
            ylow = ArrayDataSource(desc.data['value_low'])
            yhigh = ArrayDataSource(desc.data['value_high'])

            # create error bars object
            scp = plot.plots['data'][0]
            ebp = ErrorBarPlot(index=ArrayDataSource(desc.data[desc.x]),
                               value_low=ylow,
                               value_high=yhigh,
                               index_mapper=scp.index_mapper,
                               value_mapper=scp.value_mapper,
                               origin=scp.origin)
            # add error bars to plot
            plot.add(ebp)

        elif desc.data['imagedata'] is not None:
            # 2d
            plot.img_plot('imagedata', colormap=jet)
            # plot.contour_plot('imagedata')

        # pan
        pan = MTool(plot)
        plot.tools.append(pan)
        self.pans.append(pan)

        # zoom
        zoom = ZTool(plot, tool_mode='box', always_on=True, drag_button='right')
        plot.overlays.append(zoom)
        self.zooms.append(zoom)

        # save
        save = SaveTool(self.layout, always_on=True, filename="plot.pdf")
        plot.tools.append(save)

        # focus
        focus = FocusTool(self, desc)
        plot.tools.append(focus)

        return plot


    def _reset_changed(self):
        self.focus = self.descs
        self.reinit()
