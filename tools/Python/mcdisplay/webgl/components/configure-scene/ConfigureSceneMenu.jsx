import React from "react";
import GridButtons from "../grid-buttons/GridButtons";
import ViewButtons from "../view-buttons/ViewButtons";
import "./configure-scene-menu.css";
import ComponentStyler from "../component-styler/ComponentStyler";
import RaysMenu from "../rays-menu/RaysMenu";

const ConfigureSceneMenu = () => {
  return (
    <div id="configure-scene-menu">
      <h1>Menu</h1>
      <RaysMenu />
      <ComponentStyler />
      <ViewButtons />
      <GridButtons />
    </div>
  );
};

export default ConfigureSceneMenu;
