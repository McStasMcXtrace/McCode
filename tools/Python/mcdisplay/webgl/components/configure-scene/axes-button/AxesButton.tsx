import React, { useEffect } from "react";
import "../../../common.css";
import "./axes-button.css";
import { useGridContext } from "../../../Contexts/GridContext";
import { useSceneContext } from "../../../Contexts/SceneContext";

const AxesButton = () => {
  const { showAxes, toggleAxes } = useGridContext();
  const { axesRef } = useSceneContext();

  useEffect(() => {
    if (axesRef.current) {
      if (
        axesRef.current.x_axis ||
        axesRef.current.y_axis ||
        axesRef.current.z_axis
      ) {
        axesRef.current.x_axis.visible = showAxes;
        axesRef.current.y_axis.visible = showAxes;
        axesRef.current.z_axis.visible = showAxes;
      }
    }
  }, [showAxes]);

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
