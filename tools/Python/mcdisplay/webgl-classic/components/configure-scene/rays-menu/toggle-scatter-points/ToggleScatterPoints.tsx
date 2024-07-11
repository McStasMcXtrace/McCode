import React from "react";
import "../../../../common.css";
import "./toggle-scatter-points.css";
import { useRaysContext } from "../../../../Contexts/RaysContext";

const ToggleScatterPoints = () => {
  const { showScatterPoints, toggleScatterPoints } = useRaysContext();

  return (
    <div id="toggle-scatter-points" className="row">
      <button onClick={toggleScatterPoints}>
        {showScatterPoints ? "Hide Scatter points" : "Show Scatter points"}
      </button>
    </div>
  );
};

export default ToggleScatterPoints;
