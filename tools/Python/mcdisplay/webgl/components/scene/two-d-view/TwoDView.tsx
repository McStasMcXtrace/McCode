import React, { useState } from "react";
import "../../../common.css";
import "./two-d-view.css";
import Chart from "./chart/Chart";

interface TwoDViewProps {
  viewRef: React.RefObject<HTMLDivElement>;
  text: string;
  x_label?: string;
  y_label?: string;
  unit?: string;
}

const TwoDView = ({
  viewRef,
  text,
  x_label = "x",
  y_label = "y",
  unit = "m",
}: TwoDViewProps) => {
  const [aspectRatio, setAspectRatio] = useState(1.0);

  viewRef.current?.addEventListener("resize", () => {
    const view = viewRef.current;
    if (view) {
      console.log("aspectratio before: ", aspectRatio);
      setAspectRatio(view.clientWidth / view.clientHeight);
      console.log("aspectratio after: ", aspectRatio);
    }
  });

  return (
    <div id="two-d-view" className="view" ref={viewRef}>
      <Chart yAxisLabel={y_label} xAxisLabel={x_label} chartTitle={text} />
    </div>
  );
};

export default TwoDView;
