class Plot1D {
  constructor(params, svg_branch=null) {
    let p = params;
    this.params_lst = [params];
    let hdl = _draw_labels(p['w'], p['h'], p['xlabel'], p['ylabel'], p['title'], svg_branch);

    let xmin = d3.min(p['x']);
    let xmax = d3.max(p['x']);
    let ymin = d3.min(p['y']);
    let ymax = d3.max(p['y']);

    this.x_lst = [p['x']];
    this.y_lst = [p['y']];
    this.yErrData_lst = _makeErrorBarsData([p['x']], [p['y']], [p['yerr']]);

    this.pointGroup = null;
    this.last_xScale = null;
    this.last_yScale = null;

    this._draw_1d_axes(hdl.wplt, hdl.hplt, xmin, xmax, ymin, ymax, hdl.axisGroup,
      (ptGroup, xScl, yScl) => {
        this._drawPoints(xScl, yScl);
      }
    );
  }
  rePlotMany(params_lst) {
    this.x_lst = params_lst.map(p => p['x']);
    this.y_lst = params_lst.map(p => p['y']);
    let yErr_lst = params_lst.map(p => p['yerr']);
    this.yErrData_lst = _makeErrorBarsData(this.x_lst, this.y_lst, yErr_lst);

    this._drawPoints(this.last_xScale, this.last_yScale);
  }
  plotOneMore(params) {
    this.params_lst.push(params);
    this.rePlotMany(this.params_lst);
    // TODO: do something about his so we don't have to replot everything every time...
  }
  _drawPoints(xScl, yScl) {
    //const colors = d3.scaleOrdinal().range(d3.schemeCategory20);
    this.pointGroup.selectAll("path").remove();
    this.pointGroup.selectAll("line").remove();

    // draw
    for (var j=0;j<this.x_lst.length;j++) {
      let x = this.x_lst[j];
      let y = this.y_lst[j];
      let yErrData = this.yErrData_lst[j];

      // points
      this.pointGroup.append("path")
          .attr("d", d3.line()
          .x( function(d, i) { return xScl(x[i]); })
          .y( function(d, i) { return yScl(y[i]); })
          (x) // since we use the function-wide scoped x, y directly, this just causes index generation
        )
        //.attr("stroke", function(d, i) { return colors(i); })
        .attr("stroke", "black")
        .attr("stroke-width", "1px")
        .attr("fill", "none");

      // error bars
      this.pointGroup.selectAll("line")
        .data(yErrData)
        .enter()
        .append("line")
        .attr("x1", function (d) { return xScl(d.p1[0]); })
        .attr("y1", function (d) { return yScl(d.p1[1]); })
        .attr("x2", function (d) { return xScl(d.p2[0]); })
        .attr("y2", function (d) { return yScl(d.p2[1]); })
        //.attr("stroke", function(d, i) { return colors(i); })
        .attr("stroke", "black")
        .attr("stroke-width", "1px")
        .attr("fill", "none");
    }
  }
  _draw_1d_axes(w, h, xmin, xmax, ymin, ymax, pltOrigoAnchor, drawpointsCB) {
    // axis span points
    let x0 = 0;
    let y0 = h;
    let x1 = w;
    let y1 = 0;

    // zoom
    var zoom = d3.zoom()
      .on("zoom", () => {
      var new_xScale = d3.event.transform.rescaleX(xScale);
      var new_yScale = d3.event.transform.rescaleY(yScale);
      xAxisGroup.call(xAxis.scale(new_xScale));
      yAxisGroup.call(yAxis.scale(new_yScale));
      //_drawPonts_1D(pointGroup, new_xScale, new_yScale, x_lst, y_lst, yErrData_lst);
      this._drawPoints(new_xScale, new_yScale);
      this.last_xScale = new_xScale;
      this.last_yScale = new_yScale;
    });
    var view = pltOrigoAnchor.append("rect")
      .attr("width", w)
      .attr("height", h)
      .attr("style", "cursor: move; fill: none; pointer-events: all;")
      .call(zoom);

    // clip
    var clip = pltOrigoAnchor.append("clipPath")
      .attr("id", "viewClip")
      .append("rect")
      .attr("width", w)
      .attr("height", h);

    // axes
    var xScale = d3.scaleLinear()
      .domain([xmin, xmax])
      .range([x0, x1]);
    var xAxis = d3.axisBottom()
      .ticks(5)
      .tickFormat(d3.format(".1e"))
      .scale(xScale);
    var xAxisGroup = pltOrigoAnchor.append("g")
      .attr("transform", "translate(0," + y0 + ")")
      .call(xAxis);

    var yScale = d3.scaleLinear()
      .domain([ymin, ymax])
      .range([y0, y1]);
    var yAxis = d3.axisLeft()
      .ticks(5)
      .tickFormat(d3.format(".1e"))
      .scale(yScale);
    var yAxisGroup = pltOrigoAnchor.append("g")
      .attr("transform", "translate(" + x0 + ", 0)")
      .call(yAxis);

    this.last_xScale = xScale;
    this.last_yScale = yScale;
    this.pointGroup = pltOrigoAnchor.append("g")
      .attr("clip-path", "url(#viewClip)");

    // draw on initial zoom
    //let yErrData_lst = _makeErrorBarsData(x_lst, y_lst, yerr_lst);
    //var points = _drawPonts_1D(pointGroup, xScale, yScale, x_lst, y_lst, yErrData_lst);
    this._drawPoints(xScale, yScale);
  }
}

// plot 2d
function plot_2d(params, svg_branch=null) {
  var p = params
  let hdl = _draw_labels(p['w'], p['h'], p['xlabel'], p['ylabel'], p['title'], svg_branch);

  _plot_2d_data(
    hdl.wplt,
    hdl.hplt,
    p['xmin'],
    p['xmax'],
    p['ymin'],
    p['ymax'],
    p['img2dData'],
    p['imgColorbar'],
    p['cbMin'],
    p['cbMax'],
    hdl.axisGroup);
}

// private
function _draw_labels(w, h, xlabel, ylabel, title, svg_branch, plotfunc_inner) {
  if (!svg_branch) svg_branch = d3.select("body").append("svg");

  // positioning
  var margin = 5;
  var wlab = w - 2*margin;
  var hlab = h - 2*margin;

  // font size
  var fontSizeStr = "14px";

  // axis labels
  var lblGroup = svg_branch
    .attr("width", w)
    .attr("height", h)
    .append("g")
    .attr("transform", "translate(" + margin +"," + margin + ")");

  // multi-line titles
  if (!title) title="";
  var titleLines = title.split(/\r?\n/)
  var titleGrp = lblGroup
    .append("text")
    .text(titleLines[0])
    .attr("dominant-baseline", "hanging")
    .attr("text-anchor", "middle")
    .style("font-size",fontSizeStr);

  if (titleLines.length > 1) {
    for (i=1; i<titleLines.length; i++) {
      titleGrp
        .append("tspan")
        .attr("x", 0)
        .attr("dy", "1.2em")
        .text(titleLines[i]);
    }
  }

  var xLabelGrp = lblGroup
    .append("text")
    .text(xlabel)
    .attr("dominant-baseline", "middle")
    .attr("text-anchor", "middle")
    .style("font-size",fontSizeStr);

  var yLabelGrp = lblGroup
    .append("text")
    .attr("x", ylabx)
    .attr("y", ylaby)
    .text(ylabel)
    .attr("text-anchor", "middle")
    .style("font-size",fontSizeStr);

  var dt = titleGrp.node().getBBox().height;
  var dl = yLabelGrp.node().getBBox().height; // height and width are confused here, due to the rotation
  var db = xLabelGrp.node().getBBox().height;

  var wplt = wlab - 3.3*dl;
  var hplt = hlab - (1.3*dt + 2.1*db);
  var xplt = 4.0*dl;
  var yplt = dt + 0.3*dl;

  var titx = wlab/2;
  var tity = 0;//dt/2;
  var xlabx = wplt/2 + xplt;
  var xlaby = hlab - db/2;
  var ylabx = dl/2;
  var ylaby = yplt + hplt/2;

  // position labels based on what was drawn
  titleGrp.attr("transform", "translate(" + titx + "," + tity + ")");
  xLabelGrp.attr("transform", "translate(" + xlabx + "," + xlaby + ")");
  yLabelGrp.attr("transform", "rotate(-90," + ylabx + ',' + ylaby + ")  translate(" + ylabx + "," + ylaby + ")");

  var axisGroup = lblGroup
    .append("g")
    .attr("transform", "translate(" + xplt +"," + yplt + ")")
    .append("g");

  return { wplt: wplt, hplt: hplt, axisGroup: axisGroup };
}

// generates error bar vertical line coordinates from x, y and yErr, NOTE: inner loop over yErr, allowing it to be empty
function _makeErrorBarsData(x_lst, y_lst, yErr_lst) {
  // WARNING HAX
  let dataErr_lst = [];
  for (var j=0;j<yErr_lst.length;j++) {
    var dataErr = [];
    let x = x_lst[j];
    let y = y_lst[j];
    let yErr = yErr_lst[j];

    for (var i=0; i < yErr.length; i++) {
      if (yErr[i] != 0)
      dataErr.push({ "p1": [x[i], y[i]-yErr[i]], "p2" : [x[i], y[i]+yErr[i]] });
    }

    dataErr_lst.push(dataErr);
  }
  return dataErr_lst;
}

function _plot_2d_data(w, h, xmin, xmax, ymin, ymax, img2dData, imgColorbar, cbMin, cbMax, anchorElement) {
  // colorbar width
  var w_cb = 70; // this is the total width of space, image and ticks
  var w_cbimg = 15;
  var w_cbticks = 30;

  // axes lengths
  var wi = w - w_cb;
  var hi = h;
  // orego
  var x0 = 0;
  var y0 = h;
  // end coords
  var x1 = x0 + wi;
  var y1 = y0 - hi;

  // zoom
  var zoom = d3.zoom()
    .on("zoom", zoomFunction);

  function zoomFunction() {
    // create new scale ojects based on event
    var new_xScale = d3.event.transform.rescaleX(xScale);
    var new_yScale = d3.event.transform.rescaleY(yScale);

    // update axes using the new scales
    xAxisGroup.call(xAxis.scale(new_xScale));
    yAxisGroup.call(yAxis.scale(new_yScale));

    // apply pan/zoom to the image
    img.attr("transform", d3.event.transform);
  };
  var view = anchorElement.append("rect")
    .attr("class", "zoom")
    .attr("width", w)
    .attr("height", h)
    .attr("style", "cursor: move; fill: none; pointer-events: all;");

  // clip
  var clip = anchorElement.append("clipPath")
    .attr("id", "viewClip")
    .append("rect")
    .attr("width", wi)
    .attr("height", hi);

  // set up axes
  var xScale = d3.scaleLinear()
    .domain([xmin, xmax])
    .range([x0, x1]);
  var xAxis = d3.axisBottom()
    .ticks(5)
    .tickFormat(d3.format(".1e"))
    .scale(xScale);
  var xAxisGroup = anchorElement.append("g")
    .attr("transform", "translate(0," + y0 + ")")
    .call(xAxis);

  var yScale = d3.scaleLinear()
    .domain([ymin, ymax])
    .range([y0, y1]);
  var yAxis = d3.axisLeft()
    .ticks(5)
    .tickFormat(d3.format(".1e"))
    .scale(yScale);
  var yAxisGroup = anchorElement.append("g")
    .attr("transform", "translate(" + x0 + ", 0)")
    .call(yAxis);

  var pointGroup = anchorElement.append("g")
    .attr("clip-path", "url(#viewClip)")
    .call(zoom);

  var img = pointGroup.append("svg:image")
    .attr('width', wi)
    .attr('height', hi)
    .attr('preserveAspectRatio', 'none')
    .attr("xlink:href","data:image/jpg;base64," + img2dData);

  var cbScale = d3.scaleLinear()
    .domain([cbMin, cbMax])
    .range([y0, y1]);
  var cbyAxis = d3.axisRight()
    .ticks(5)
    .tickFormat(d3.format(".1e"))
    .scale(cbScale);
  var cbAxisGroup = anchorElement.append("g")
    .attr("transform", "translate(" + (x1 + w_cb - w_cbimg - w_cbticks) + ", 0)")
    .call(cbyAxis);
  var cb = cbAxisGroup.append("svg:image")
    .attr('width', w_cbimg)
    .attr('height', hi)
    .attr("transform", "translate(" + (-w_cbimg) + ", 0)")
    .attr('preserveAspectRatio', 'none')
    .attr("xlink:href","data:image/jpg;base64," + imgColorbar);
}
