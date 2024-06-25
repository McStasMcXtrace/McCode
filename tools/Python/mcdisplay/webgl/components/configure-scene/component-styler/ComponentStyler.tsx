import React, { useEffect, useState } from "react";
import "../../../common.css";
import "./component-styler.css";
import { useComponentsContext } from "../../../Contexts/ComponentsContext";
import DropDown from "./dropdown/DropDown";
import ColorPicker from "./color-picker/ColorPicker";
import { DrawCall, Component } from "../../../model/Component";

const ComponentStyler = () => {
  const fallBackComponent: Component = {
    id: 0,
    name: "Loading instrument",
    m4: [],
    drawcalls: { key: "", args: [] },
    color: "#1a73e8",
    transparency: 1,
  };
  const { components, setComponents } = useComponentsContext();
  const [currentComponent, setCurrentComponent] =
    useState<Component>(fallBackComponent);

  useEffect(() => {
    if (
      components.length > 0 &&
      currentComponent.name === "Loading instrument"
    ) {
      setCurrentComponent(components[1]);
    }
  }, [components, currentComponent]);

  return (
    <div id="component-styler" className="row">
      <p className="bold">Edit:</p>
      <DropDown
        currentComponent={currentComponent}
        setCurrentComponent={setCurrentComponent}
        components={components}
      />
      <ColorPicker
        setCurrentComponent={setCurrentComponent}
        components={components}
        setComponents={setComponents}
        currentComponent={currentComponent}
      />
    </div>
  );
};

export default ComponentStyler;
