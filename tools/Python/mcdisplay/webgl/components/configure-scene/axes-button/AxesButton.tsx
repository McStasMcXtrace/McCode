import React from "react";
import "../../../common.css";
import "./axes-button.css";
import { useGridContext } from "../../../Contexts/GridContext";

const AxesButton = () => {
  const { showAxes, toggleAxes } = useGridContext();
  return (
    <button
      id="axes-button"
      onClick={toggleAxes}
      className={`row ${showAxes ? "active" : ""}`}
    >
      <p id="x"> X</p>
      <p id="y"> Y</p>
      <p id="z"> Z</p>
    </button>
  );
};

export default AxesButton;
