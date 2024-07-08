import React from "react";
import "../../../common.css";
import "./background-color-button.css";
import { useAppContext } from "../../../Contexts/AppContext";

const BackgroundColorButton = () => {
  const { backgroundColor, toggleBackgroundColor } = useAppContext();

  const changeColor = () => {
    toggleBackgroundColor();
  };

  return (
    <button id="background-color-button" onClick={changeColor}>
      {backgroundColor ? "Darken" : "Lighten"}
    </button>
  );
};

export default BackgroundColorButton;
