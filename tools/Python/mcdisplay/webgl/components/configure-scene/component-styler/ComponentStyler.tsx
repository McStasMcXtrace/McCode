import React, { useEffect, useState } from "react";
import "../../../common.css";
import "./component-styler.css";
import { useInstrumentContext } from "../../../Contexts/InstrumentContext";
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
  const { instrument, setInstrument } = useInstrumentContext();
  const [currentComponent, setCurrentComponent] =
    useState<Component>(fallBackComponent);

  useEffect(() => {
    if (
      instrument.components.length > 0 &&
      currentComponent.name === "Loading instrument"
    ) {
      setCurrentComponent(instrument.components[1]);
    }
  }, [instrument.components, currentComponent]);

  return (
    <div id="component-styler" className="row">
      <p className="bold">Edit:</p>
      <DropDown
        currentComponent={currentComponent}
        setCurrentComponent={setCurrentComponent}
        components={instrument.components}
      />
      <ColorPicker
        setCurrentComponent={setCurrentComponent}
        currentComponent={currentComponent}
      />
    </div>
  );
};

export default ComponentStyler;
