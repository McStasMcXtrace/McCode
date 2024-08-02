import React from "react";
import "../../../common.css";
import "./rays-menu.css";
import ToggleRays from "./toggle-rays/ToggleRays";

const RaysMenu = () => {
  return (
    <div id="rays-menu" className="row">
      <ToggleRays />
    </div>
  );
};

export default RaysMenu;
