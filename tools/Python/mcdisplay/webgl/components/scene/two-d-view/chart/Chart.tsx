import React from "react";
import "../../../../common.css";
import Plot from "react-plotly.js";
import "./chart.css";
import { usePlotRangeContext } from "../../../../Contexts/PlotRangeContext";

interface ChartProps {
  chartTitle: string;
  xAxisLabel: string;
  yAxisLabel: string;
}

const Chart: React.FC<ChartProps> = ({
  chartTitle,
  xAxisLabel,
  yAxisLabel,
}) => {
  const { plotlyRanges } = usePlotRangeContext();

  const layout = {
    title: chartTitle,
    xaxis: {
      title: xAxisLabel,
      showgrid: false,
      zeroline: false,
      showline: false,
      range: plotlyRanges[chartTitle]?.xaxis,
    },
    yaxis: {
      title: {
        text: yAxisLabel,
        standoff: 10, // Adjust this value to move the title
      },
      showgrid: false,
      zeroline: false,
      showline: false,
      range: plotlyRanges[chartTitle]?.yaxis,
    },
    paper_bgcolor: "rgba(0,0,0,0)",
    plot_bgcolor: "rgba(0,0,0,0)",
    autosize: true,
    margin: {
      l: 30, // left margin
      r: 15, // right margin
      b: 15, // bottom margin
      t: 15, // top margin
      pad: 0, // padding
    },
    dragmode: "pan",
  };

  const config = {
    displayModeBar: true,
    modeBarButtonsToRemove: [
      "toImage",
      "zoom2d",
      "select2d",
      "lasso2d",
      "zoomIn2d",
      "zoomOut2d",
      "autoScale2d",
      "resetScale2d",
    ],
    displaylogo: false,
    scrollZoom: false,
    staticPlot: false,
  };

  return (
    <div id={`plotlyDiv-${chartTitle}`} className="chart">
      <Plot
        data={[]}
        layout={layout}
        config={config}
        useResizeHandler
        style={{ width: "100%", height: "100%" }}
      />
    </div>
  );
};

export default Chart;
