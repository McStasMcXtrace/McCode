import React from "react";
import { useCameraContext } from "../../../Contexts/CameraContext";
import "../../common.css";
import "./view-buttons.css";

const ViewButtons = () => {
  const { setCamPos, camPosHome, camPosSide, camPosTop } = useCameraContext();

  return (
    <div id="view-buttons" className="row">
      <p className="bold">Reset View: </p>
      <button onClick={() => setCamPos(camPosHome)}>Home</button>
      <button onClick={() => setCamPos(camPosSide)}>Side</button>
      <button onClick={() => setCamPos(camPosTop)}>Top</button>
    </div>
  );
};

export default ViewButtons;
