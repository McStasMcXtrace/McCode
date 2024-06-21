import React, { useEffect, useState } from "react";
import "../../../common.css";
import "./dropdown.css";
import DropDownButton from "../dropdown-button/DropDownButton";
import { Component } from "../../../../model/Component";

interface DropDownProps {
  components: Component[];
  currentComponent: Component;
  setCurrentComponent: React.Dispatch<React.SetStateAction<Component>>;
}

const DropDown = ({
  components,
  currentComponent,
  setCurrentComponent,
}: DropDownProps) => {
  const [open, setOpen] = useState(false);

  const handleOpen = () => {
    setOpen(!open);
  };

  const handleComponentSelect = (component: Component) => {
    setCurrentComponent(component);
    setOpen(false);
  };

  return (
    <div id="dropdown" className="">
      <DropDownButton
        text={currentComponent.name}
        handleOpen={handleOpen}
        color={currentComponent.color}
      />
      {open ? (
        <ul className="dropdown-menu">
          {components.map((component, index) => (
            <li
              key={index}
              style={{ backgroundColor: component.color }}
              onClick={() => handleComponentSelect(component)}
            >
              {component.name}
            </li>
          ))}
        </ul>
      ) : null}
    </div>
  );
};

export default DropDown;
