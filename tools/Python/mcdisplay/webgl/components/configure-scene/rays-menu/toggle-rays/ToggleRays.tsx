import React from "react";
import "../../../../common.css";
import "./toggle-rays.css";
import { useRaysContext } from "../../../../Contexts/RaysContext";
import ToggleScatterPoints from "../toggle-scatter-points/ToggleScatterPoints";
import RaysPlayback from "../rays-playback/RaysPlayback";

const ToggleRays = () => {
  const { showRays, toggleRays } = useRaysContext();

  return (
    <div id="toggle-rays" className="row">
      <button onClick={toggleRays}>
        {showRays ? "Hide Rays" : "Show Rays"}
      </button>
      {showRays ? <RaysPlayback /> : null}
      {showRays ? <ToggleScatterPoints /> : null}
    </div>
  );
};

export default ToggleRays;
