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
from chaco.tools.line_inspector import LineInspector

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
            # reset current plot
            self.mclayout.reinit()
        else:
            # focus on chosen plot
            self.mclayout.focus = [self.desc]
        # reinit layout
        self.mclayout.reinit()

    def normal_key_pressed(self, event):
        if event.character == 'l':
            self.desc.log = not self.desc.log
            self.mclayout.reinit()


class Snapper(LineInspector):
    def __init__(self, mclayout, plot, **kwargs):
        super(Snapper, self).__init__(plot, axis='index_x', **kwargs)
        self.layout = mclayout
        self.plot = plot
        self.pos = None
        self.enabled = False
        self.title = plot.title


    def normal_key_pressed(self, event):
        if event.character == 't':
            self.enabled = not self.enabled
            if not self.enabled:
                self.pos = None
                self.plot.title = self.title
            self.plot.request_redraw()


    def normal_mouse_move(self, event):
        if not self.enabled:
            return

        mx = self.plot.index_mapper.map_data(event.x)

        px = self.plot.data[self.plot.x_axis.title][0]
        for i, x in enumerate(self.plot.data[self.plot.x_axis.title]):
            if x > mx:
                break
            px = x


        if (px - mx) ** 2 < (x - mx)**2:
            i -= 1
            mx = px
        else:
            mx = x

        my = self.plot.data[self.plot.y_axis.title][i]

        x = self.plot.index_mapper.map_screen(mx)
        y = self.plot.value_mapper.map_screen(my)

        self.pos = (x, y)
        self.plot.title = '%s (%.4f, %.4f)' % (self.title, x, y)

        self.plot.request_redraw()


    def draw(self, gc, view_bounds=None):
        if self.pos is None or not self.enabled:
            self._last_position = None
        else:
            x, y = self.pos
            super(Snapper, self)._draw_vertical_line(gc, x)
            super(Snapper, self)._draw_horizontal_line(gc, y)

        super(Snapper, self).draw(gc, view_bounds)


class PlotDesc(object):
    def __init__(self, x, y, data, title, log=False, **kwargs):
        self.x = x
        self.y = y
        self.data = data
        self.title = title
        self.log = log
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

        plot.title = desc.title
        plot.x_axis.title = desc.x
        plot.y_axis.title = desc.y

        log = desc.log

        if None not in (desc.data['value_low'], desc.data['value_high']):
            # 1d
            value_scale = 'log' if log else 'linear'
            plot.plot((desc.x, desc.y), name='data',
                      value_scale=value_scale, **desc.params)

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
            data_name = 'imagedata_log' if log else 'imagedata'
            plot.img_plot(data_name, colormap=jet)
            # plot.contour_plot('imagedata')

        # pan
        pan = MTool(plot)
        plot.tools.append(pan)
        self.pans.append(pan)

        # zoom
        zoom = ZTool(plot, tool_mode='box', always_on=True, drag_button='right')
        plot.overlays.append(zoom)
        self.zooms.append(zoom)

        # line inspector
        plot.overlays.append(Snapper(self.layout, plot))

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
