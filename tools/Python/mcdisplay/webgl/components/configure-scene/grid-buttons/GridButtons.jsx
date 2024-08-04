import React, { useEffect } from "react";
import "../../../common.css";
import { useGridContext } from "../../../Contexts/GridContext";
import { useSceneContext } from "../../../Contexts/SceneContext";

const GridButtons = () => {
  const { showXY, showXZ, showYZ, toggleXY, toggleXZ, toggleYZ, gridSize } =
    useGridContext();
  const { gridsRef, sceneRef } = useSceneContext();

  useEffect(() => {
    if (gridsRef.current) {
      if (gridsRef.current.gridXY) {
        gridsRef.current.gridXY.visible = showXY;
      }
      if (gridsRef.current.gridXZ) {
        gridsRef.current.gridXZ.visible = showXZ;
      }
      if (gridsRef.current.gridYZ) {
        gridsRef.current.gridYZ.visible = showYZ;
      }
    }
  }, [showXY, showXZ, showYZ]);

  return (
    <div id="grid-buttons" className="row">
      <p className="bold">Toggle Grids: </p>
      <button onClick={toggleXY} className={showXY ? "active" : ""}>
        XY
      </button>
      <button onClick={toggleXZ} className={showXZ ? "active" : ""}>
        XZ
      </button>
      <button onClick={toggleYZ} className={showYZ ? "active" : ""}>
        YZ
      </button>
    </div>
  );
};

export default GridButtons;
