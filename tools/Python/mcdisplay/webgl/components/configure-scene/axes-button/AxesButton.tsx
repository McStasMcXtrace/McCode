import React from "react";
import "../../../common.css";
import "./axes-button.css";
import { useGridContext } from "../../../Contexts/GridContext";

const AxesButton = () => {
  const { showAxes, toggleAxes } = useGridContext();
  return (
    <div id="axes-button" className="">
      <button onClick={toggleAxes} className="row">
        {showAxes ? "Hide Axes  " : "Show Axes  "}
        <p id="x"> X</p>
        <p id="y"> Y</p>
        <p id="z"> Z</p>
      </button>
    </div>
  );
};

export default AxesButton;
