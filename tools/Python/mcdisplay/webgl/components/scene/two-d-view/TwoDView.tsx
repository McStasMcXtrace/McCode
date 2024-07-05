import React from "react";
import "../../../common.css";
import "./two-d-view.css";

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
  return (
    <div id="two-d-view" className="view" ref={viewRef}>
      <p className="view-name gray-color">{text}</p>
      <div className="y-axis gray-color">
        {y_label}[{unit}]
      </div>
      <div className="x-axis gray-color">
        {x_label}[{unit}]
      </div>
    </div>
  );
};

export default TwoDView;
