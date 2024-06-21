import React, { useEffect, useRef } from "react";
import { useGridContext } from "../../Contexts/GridContext";
import { useCameraContext } from "../../Contexts/CameraContext";
import "./three-canvas.css";
import {
  initializeScene,
  initializeCamera,
  initializeRenderer,
  initializeGrids,
  initializeControls,
  initializeDirectionalLight,
  initializeAmbientLight,
} from "./utils/initializeScene";
import { useComponentsContext } from "../../Contexts/ComponentsContext";
import { clearComponents, loadComponents } from "../../Contexts/addComponents";
import { useRaysContext } from "../../Contexts/RaysContext";
import {
  setScatterPointsVisible,
  setScatterPointsInvisible,
  addRays,
  setRaysVisible,
  setRaysInvisible,
} from "../../Contexts/addRays";

const ThreeCanvas = () => {
  const { showXY, showXZ, showYZ, gridSize, gridDivisions } = useGridContext();
  const { camPos } = useCameraContext();
  const { components, setComponents } = useComponentsContext();
  const { showScatterPoints, showRays, rays } = useRaysContext();

  const gridsRef = useRef({ gridXY: null, gridXZ: null, gridYZ: null });

  const cameraRef = useRef(null);
  const controlsRef = useRef(null);
  const containerRef = useRef(null);
  const rendererRef = useRef(null);
  const sceneRef = useRef(null);

  useEffect(() => {
    const container = containerRef.current;
    if (!container) return;
    const margin = 20;
    const width = (container.clientWidth || window.innerWidth) - margin;
    const height = (container.clientHeight || window.innerHeight) - margin;

    const scene = initializeScene();
    sceneRef.current = scene;
    const camera = initializeCamera(width, height, camPos);
    cameraRef.current = camera;
    const renderer = initializeRenderer(width, height, container);
    rendererRef.current = renderer;
    const grids = initializeGrids(scene, gridSize, gridDivisions);
    gridsRef.current = grids;
    const controls = initializeControls(camera, renderer);
    loadComponents(scene, components);
    setComponents(components);
    addRays(scene, rays, components);
    initializeDirectionalLight(scene);
    initializeAmbientLight(scene);
    controlsRef.current = controls;

    function animate() {
      requestAnimationFrame(animate);
      controls.update();
      renderer.render(scene, camera);
    }

    animate();

    const handleResize = () => {
      const newWidth = (container.clientWidth || window.innerWidth) - margin;
      const newHeight = (container.clientHeight || window.innerHeight) - margin;
      camera.aspect = newWidth / newHeight;
      camera.updateProjectionMatrix();
      renderer.setSize(newWidth, newHeight);
    };

    window.addEventListener("resize", handleResize);

    return () => {
      if (container) {
        container.removeChild(renderer.domElement);
      }
      window.removeEventListener("resize", handleResize);
    };
  }, []);

  useEffect(() => {
    if (gridsRef.current.gridXY) {
      gridsRef.current.gridXY.visible = showXY;
    }
    if (gridsRef.current.gridXZ) {
      gridsRef.current.gridXZ.visible = showXZ;
    }
    if (gridsRef.current.gridYZ) {
      gridsRef.current.gridYZ.visible = showYZ;
    }
  }, [showXY, showXZ, showYZ]);

  useEffect(() => {
    if (cameraRef.current) {
      cameraRef.current.position.set(camPos.x, camPos.y, camPos.z);
      if (controlsRef.current) {
        controlsRef.current.update();
      }
    }
  }, [camPos]);

  useEffect(() => {
    clearComponents(sceneRef.current);
    loadComponents(sceneRef.current, components);
    rendererRef.current.render(sceneRef.current, cameraRef.current);
  }, [components]);

  useEffect(() => {
    if (!showRays) {
      setRaysInvisible(sceneRef.current);
      rendererRef.current.render(sceneRef.current, cameraRef.current);
    } else {
      setRaysVisible(sceneRef.current);
      rendererRef.current.render(sceneRef.current, cameraRef.current);
    }
  }, [showRays]);

  useEffect(() => {
    if (!showScatterPoints) {
      setScatterPointsInvisible(sceneRef.current);
      rendererRef.current.render(sceneRef.current, cameraRef.current);
    } else {
      setScatterPointsVisible(sceneRef.current);
      rendererRef.current.render(sceneRef.current, cameraRef.current);
    }
  }, [showScatterPoints]);

  return <div id="canvas-container" ref={containerRef}></div>;
};

export default ThreeCanvas;
