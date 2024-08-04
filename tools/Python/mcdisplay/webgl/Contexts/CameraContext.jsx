import React, { createContext, useContext, useState } from "react";
import { Vector3 } from "three";

const CameraContext = createContext({
  camPos: new Vector3(10, 20, 30),
  camPosHome: new Vector3(10, 20, 30),
  camPosSide: new Vector3(0, 20, 0),
  camPosTop: new Vector3(0, 0, 30),
  setCamPos: () => {},
  setCamPosHome: () => {},
  setCamPosSide: () => {},
  setCamPosTop: () => {},
});

export const CameraProvider = ({ children }) => {
  const [camPos, setCamPos] = useState(new Vector3(0, 10, 0));
  const [camPosHome, setCamPosHome] = useState(new Vector3(0, 10, 0));
  const [camPosSide, setCamPosSide] = useState(new Vector3(0, 0, 30));
  const [camPosTop, setCamPosTop] = useState(new Vector3(0, 30, 0));

  const handleSetCamPos = (position) => {
    setCamPos(position);
  };

  return (
    <CameraContext.Provider
      value={{
        camPos,
        setCamPos: handleSetCamPos,
        camPosHome,
        setCamPosHome,
        camPosSide,
        setCamPosSide,
        camPosTop,
        setCamPosTop,
      }}
    >
      {children}
    </CameraContext.Provider>
  );
};

export const useCameraContext = () => useContext(CameraContext);
