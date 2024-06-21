import React from "react";
import "../../../common.css";
import { useGridContext } from "../../../Contexts/GridContext";

const GridButtons = () => {
  const { showXY, showXZ, showYZ, toggleXY, toggleXZ, toggleYZ } =
    useGridContext();

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
