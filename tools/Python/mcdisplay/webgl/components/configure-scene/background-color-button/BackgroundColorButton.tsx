import React, { useState } from "react";
import "../../../common.css";
import "./background-color-button.css";
import { useSceneContext } from "../../../Contexts/SceneContext";
import * as THREE from "three";

const BackgroundColorButton = () => {
  const { sceneRef } = useSceneContext();
  const [backgroundColor, setBackgroundColor] = useState(false);

  const changeColor = () => {
    setBackgroundColor((prevBackgroundColor) => {
      if (prevBackgroundColor) {
        sceneRef.current.background = new THREE.Color(0xffffff);
      } else {
        sceneRef.current.background = new THREE.Color(0x000000);
      }
      return !prevBackgroundColor;
    });
  };

  return (
    <button id="background-color-button" onClick={changeColor}>
      {backgroundColor ? "Lighten" : "Darken"}
    </button>
  );
};

export default BackgroundColorButton;
