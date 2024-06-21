import React, { useState } from "react";
import "../../../common.css";
import "./component-styler.css";
import { useComponentsContext } from "../../../Contexts/ComponentsContext";
import DropDown from "./dropdown/DropDown";
import ColorPicker from "./color-picker/ColorPicker";
import { Component } from "../../../model/Component";

const ComponentStyler = () => {
  const { components, setComponents } = useComponentsContext();
  const [currentComponent, setCurrentComponent] = useState<Component>(
    components[1]
  );

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
