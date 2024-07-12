import React from "react";
import "../../../../common.css";
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
  return <div id="chart" className=""></div>;
};

export default Chart;
