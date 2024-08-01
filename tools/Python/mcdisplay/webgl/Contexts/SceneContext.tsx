import React, { createContext, useContext, ReactNode, useRef } from "react";
import * as THREE from "three";
import { initializeScene } from "../components/scene/initializeScene";

type SceneContextType = {
  gridsRef: React.MutableRefObject<any>;
  axesRef: React.MutableRefObject<any>;
  containerRef: React.MutableRefObject<any>;
  primaryCameraRef: React.MutableRefObject<any>;
  primaryControlsRef: React.MutableRefObject<any>;
  primaryViewRef: React.MutableRefObject<any>;
  TopView2DRef: React.MutableRefObject<any>;
  SideView2DRef: React.MutableRefObject<any>;
  BackView2DRef: React.MutableRefObject<any>;
  rendererRef: React.MutableRefObject<any>;
  sceneRef: React.MutableRefObject<any>;
};

const SceneContext = createContext<SceneContextType>({
  gridsRef: { current: null },
  axesRef: { current: null },
  containerRef: { current: null },
  primaryCameraRef: { current: null },
  primaryControlsRef: { current: null },
  primaryViewRef: { current: null },
  TopView2DRef: { current: null },
  SideView2DRef: { current: null },
  BackView2DRef: { current: null },
  rendererRef: { current: null },
  sceneRef: { current: null },
});

interface SceneProviderProps {
  children: ReactNode;
}

export const SceneProvider: React.FC<SceneProviderProps> = ({ children }) => {
  const gridsRef = useRef({ gridXY: null, gridXZ: null, gridYZ: null });
  const axesRef = useRef({ x_axis: null, y_axis: null, z_axis: null });
  const containerRef = useRef(null);
  const primaryCameraRef = useRef(null);
  const primaryControlsRef = useRef(null);
  const primaryViewRef = useRef(null);
  const TopView2DRef = useRef(null);
  const SideView2DRef = useRef(null);
  const BackView2DRef = useRef(null);
  const rendererRef = useRef(null);
  const sceneRef = useRef(initializeScene());

  return (
    <SceneContext.Provider
      value={{
        gridsRef,
        axesRef,
        containerRef,
        primaryCameraRef,
        primaryControlsRef,
        primaryViewRef,
        TopView2DRef,
        SideView2DRef,
        BackView2DRef,
        rendererRef,
        sceneRef,
      }}
    >
      {children}
    </SceneContext.Provider>
  );
};

export const useSceneContext = () => useContext(SceneContext);
