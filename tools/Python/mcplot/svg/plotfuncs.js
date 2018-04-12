// public plot 1d data
function plot_1d(params, svg_branch=null) {
  var p = params
  _plot_labels(
    p['w'], p['h'], p['xlabel'], p['ylabel'], p['title'], svg_branch,
    function(w, h, anchor) { _plot_1d_data(w, h, p['x'], p['y'], p['yerr'], anchor) });
}
// public plot 2d data
function plot_2d(params, svg_branch=null) {
  var p = params
  _plot_labels(
    p['w'], p['h'], p['xlabel'], p['ylabel'], p['title'], svg_branch,
    function(w, h, anchor) { _plot_2d_data(w, h, p['xmin'], p['xmax'], p['ymin'], p['ymax'], p['img2dData'], p['imgColorbar'], p['cbMin'], p['cbMax'], anchor) });
}

function _plot_labels(w, h, xlabel, ylabel, title, svg_branch, plotfunc_inner) {
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

  plotfunc_inner(wplt, hplt, axisGroup);
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

function _plot_1d_data(w, h, x, y, yerr, anchorElement) {
  // axis size fractions and orego placement fraction
  var xax_frac = 1;
  var yax_frac = 1;
  var x0_frac = 0;
  var y0_frac = 0;

  // axes lengths
  var wi = xax_frac*w;
  var hi = yax_frac*h;

  // orego
  var x0 = x0_frac*w;
  var y0 = h - y0_frac*h;

  // axes end coords
  var x1 = x0 + wi;
  var y1 = y0 - hi;

  var data = []
  var dataErr = []
  for (var i=0; i < x.length; i++) {
    data.push({ "x" : x[i], "y" : y[i] });
    if (yerr[i] != 0)
      dataErr.push({ "p1": [x[i], y[i]-yerr[i]], "p2" : [x[i], y[i]+yerr[i]] });
  }

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

    // clear & redraw based on the scaled data
    drawPoints(new_xScale, new_yScale);
  };
  var view = anchorElement.append("rect")
    .attr("class", "zoom")
    .attr("width", w)
    .attr("height", h)
    .attr("style", "cursor: move; fill: none; pointer-events: all;")
    .call(zoom);

  // clip
  var clip = anchorElement.append("clipPath")
    .attr("id", "viewClip")
    .append("rect")
    .attr("width", w)
    .attr("height", h);

  // set up axes
  var xScale = d3.scaleLinear()
    .domain([d3.min(x), d3.max(x)])
    .range([x0, x1]);
  var xAxis = d3.axisBottom()
    .ticks(5)
    .tickFormat(d3.format(".1e"))
    .scale(xScale);
  var xAxisGroup = anchorElement.append("g")
    .attr("transform", "translate(0," + y0 + ")")
    .call(xAxis);

  var yScale = d3.scaleLinear()
    .domain([d3.min(y), d3.max(y)])
    .range([y0, y1]);
  var yAxis = d3.axisLeft()
    .ticks(5)
    .tickFormat(d3.format(".1e"))
    .scale(yScale);
  var yAxisGroup = anchorElement.append("g")
    .attr("transform", "translate(" + x0 + ", 0)")
    .call(yAxis);

  // enter data points
  var pointGroup = anchorElement.append("g")
    .attr("clip-path", "url(#viewClip)");

  // draw data on initial zoom
  var points = drawPoints(xScale, yScale);

  function drawPoints(xScl, yScl) {
    // data draw functions
    function getLineFunc(xs, ys) {
      return d3.line()
      .x(function(d) { return xs(d.x); })
      .y(function(d) { return ys(d.y); });
    }
    var lf = getLineFunc(xScl, yScl);
    // draw graph
    pointGroup.selectAll("path").remove();
    pointGroup.selectAll("line").remove();
    graph = pointGroup.append("path")
      .attr("d", lf(data))
      .attr("stroke", "black")
      .attr("stroke-width", "1px")
      .attr("fill", "none");

    // draw error bars
    pointGroup.selectAll("line")
      .data(dataErr)
      .enter()
      .append("line")
      .attr("x1", function (d) { return xScl(d.p1[0]); })
      .attr("y1", function (d) { return yScl(d.p1[1]); })
      .attr("x2", function (d) { return xScl(d.p2[0]); })
      .attr("y2", function (d) { return yScl(d.p2[1]); })
      .attr("stroke", "black")
      .attr("stroke-width", "1px")
      .attr("fill", "none");
  }
}
