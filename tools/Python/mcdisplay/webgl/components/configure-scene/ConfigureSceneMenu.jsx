import React from "react";
import GridButtons from "./grid-buttons/GridButtons";
import ViewButtons from "./view-buttons/ViewButtons";
import "./configure-scene-menu.css";
import ComponentStyler from "./component-styler/ComponentStyler";
import RaysMenu from "./rays-menu/RaysMenu";

const ConfigureSceneMenu = () => {
  return (
    <div id="configure-scene-menu">
      <RaysMenu />
      <ComponentStyler />
      <ViewButtons />
      <GridButtons />
    </div>
  );
};

export default ConfigureSceneMenu;
