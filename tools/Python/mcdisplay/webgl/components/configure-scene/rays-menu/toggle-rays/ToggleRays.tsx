import React from "react";
import "../../../../common.css";
import "./toggle-rays.css";
import { useRaysContext } from "../../../../Contexts/RaysContext";
import ToggleScatterPoints from "../toggle-scatter-points/ToggleScatterPoints";
import RaysPlayback from "../rays-playback/RaysPlayback";
import ShowAllRays from "../show-all-rays/ShowAllRays";

const ToggleRays = () => {
  const { showAllRays, toggleShowAllRays, showRays, toggleRays } =
    useRaysContext();
  return (
    <div id="toggle-rays" className="row">
      <button onClick={toggleRays}>
        {showRays ? "Hide Rays" : "Show Rays"}
      </button>
      {showRays && showAllRays ? <ShowAllRays text="Show PlayBack" /> : null}
      {showRays && !showAllRays ? <ShowAllRays text="Show All" /> : null}
      {showRays && !showAllRays ? <RaysPlayback /> : null}
      {showRays ? <ToggleScatterPoints /> : null}
    </div>
  );
};

export default ToggleRays;
