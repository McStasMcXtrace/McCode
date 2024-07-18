import React from "react";
import "../../../../common.css";
import Plot from "react-plotly.js";
import "./chart.css";

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
  const layout = {
    title: chartTitle,
    xaxis: {
      title: xAxisLabel,
      showgrid: true,
      zeroline: false,
      showline: true,
      range: [0, 100], // Set a range from data later
    },
    yaxis: {
      title: {
        text: yAxisLabel,
        standoff: 10, // Adjust this value to move the title
      },
      showgrid: true,
      zeroline: false,
      showline: true,
      range: [-50, 50], // Set a range from data later
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
  };

  return (
    <div id="chart" className="">
      <Plot
        data={[]}
        layout={layout}
        useResizeHandler
        style={{ width: "100%", height: "100%" }}
      />
    </div>
  );
};

export default Chart;
