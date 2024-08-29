import React, { useState } from "react";
import "../../../../common.css";
import "./toggle-scatter-points.css";
import {
  setScatterPointsInvisible,
  setScatterPointsVisible,
} from "../../../../Contexts/addRays";
import { useSceneContext } from "../../../../Contexts/SceneContext";

const ToggleScatterPoints = () => {
  const { sceneRef } = useSceneContext();
  const [showScatterPoints, setShowScatterPoints] = useState(true);

  const handleClick = () => {
    setShowScatterPoints((prevShowScatterPoints) => {
      if (prevShowScatterPoints) {
        setScatterPointsInvisible(sceneRef.current);
      } else {
        setScatterPointsVisible(sceneRef.current);
      }
      return !prevShowScatterPoints;
    });
  };

  return (
    <div id="toggle-scatter-points" className="row">
      <button onClick={handleClick}>
        {showScatterPoints ? "Hide Scatter points" : "Show Scatter points"}
      </button>
    </div>
  );
};

export default ToggleScatterPoints;
