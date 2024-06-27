import React, { useState, useEffect, useRef } from "react";
import { useGridContext } from "../../Contexts/GridContext";
import { useCameraContext } from "../../Contexts/CameraContext";
import "./three-canvas.css";
import {
  initializeScene,
  initializeCamera,
  initializeRenderer,
  addGrids,
  initializeControls,
  initializeDirectionalLight,
  initializeAmbientLight,
} from "./utils/initializeScene";
import { useComponentsContext } from "../../Contexts/ComponentsContext";
import { clearComponents, loadComponents } from "../../Contexts/addComponents";
import { useRaysContext } from "../../Contexts/RaysContext";
import {
  setRayVisibility,
  setScatterPointsVisible,
  setScatterPointsInvisible,
  addRays,
  setRaysVisible,
  setRaysInvisible,
} from "../../Contexts/addRays";
import { useAppContext } from "../../Contexts/AppContext";
import * as THREE from "three";

const ThreeCanvas = () => {
  const { showXY, showXZ, showYZ, gridSize, gridDivisions } = useGridContext();
  const { camPos, setCamPosSide, setCamPosTop, setCamPosHome } =
    useCameraContext();
  const { components, setComponents } = useComponentsContext();
  const {
    showAllRays,
    toggleShowAllRays,
    play,
    setPlay,
    prevRayIndex,
    currentRayIndex,
    setCurrentRayIndex,
    showScatterPoints,
    showRays,
    rays,
    handleNextClick,
  } = useRaysContext();
  const { loading, setLoading } = useAppContext();
  const [hoverInfo, setHoverInfo] = useState("");
  const gridsRef = useRef({ gridXY: null, gridXZ: null, gridYZ: null });
  const raycaster = new THREE.Raycaster();
  const pointer = new THREE.Vector2();
  
  function onPointerMove(event) {
    // calculate pointer position in normalized device coordinates
    // (-1 to +1) for both components

    pointer.x = (event.clientX / window.innerWidth) * 2 - 1;
    pointer.y = -(event.clientY / window.innerHeight) * 2 + 1;
  }
  const cameraRef = useRef(null);
  const controlsRef = useRef(null);
  const containerRef = useRef(null);
  const rendererRef = useRef(null);
  const sceneRef = useRef(null);
  const playRef = useRef(play);

  const views = [
    // Main view Perspective camera
    {
      left: 0.0,
      bottom: 0.5,
      width: 1.0,
      height: 0.5,
      camera: "PerspectiveCamera",
      initialCamPos: [10, 20, 30],
    },
    // PyQtGraph replacements using Ortographic cameras
    // Top view - XZ
    {
      left: 0.0,
      bottom: 0.25,
      width: 1.0,
      height: 0.25,
      camera: "OrthographicCamera",
      initialCamPos: [0, gridSize, 0],
    },
    // Back view - XY
    {
      left: 0.0,
      bottom: 0.0,
      width: 0.5,
      height: 0.25,
      camera: "OrthographicCamera",
      initialCamPos: [gridSize, 0, 0],
    },
    //Side view - YZ
    {
      left: 0.5,
      bottom: 0.0,
      width: 0.5,
      height: 0.25,
      camera: "OrthographicCamera",
      initialCamPos: [0, 0, gridSize],
    },
  ];
  const insertText = (textarea, text) => {
    // Get the current cursor position
    const position = textarea.selectionStart;

    // Get the text before and after the cursor position
    const before = textarea.value.substring(0, position);
    const after = textarea.value.substring(position, textarea.value.length);

    // Insert the new text at the cursor position
    textarea.value = before + text + after;

    // Set the cursor position to after the newly inserted text
    textarea.selectionStart = textarea.selectionEnd = position + text.length;
};

  function render() {
    /*
    raycaster.setFromCamera(pointer, cameraRef.current);
    const intersects = raycaster.intersectObjects(sceneRef.current.children);
    let currHoverInfo = "";
    for (let i = 0; i < intersects.length; i++) {
      if (
        intersects[i].object.visible === true &&
        !intersects[i].object.name.includes("grid")
      ) {
        currHoverInfo = intersects[i].object.name + " ";
        if(intersects[i].object.name==="" && intersects[i].object.parent.name !== undefined && intersects[i].object.parent.visible === true){          
          currHoverInfo += intersects[i].object.parent.name + " ";
        }
      }
    }
    if(currHoverInfo !== undefined) console.log(currHoverInfo);
    insertText(document.getElementById("hover-info"), currHoverInfo);
    */
    /*rendererRef.current.setViewPort(left, bottom, width, height);
    views.forEach(element => {
      
    });
    */
    controlsRef.current.update();
    rendererRef.current.render(sceneRef.current, cameraRef.current);
  }

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
    const controls = initializeControls(camera, renderer);

    initializeDirectionalLight(scene);
    initializeAmbientLight(scene);
    controlsRef.current = controls;

    function animate() {
      requestAnimationFrame(animate);
      render();
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
        controlsRef.current.target.set(0, 0, camPos.z);
        controlsRef.current.update();
      }
    }
  }, [camPos]);

  useEffect(() => {
    clearComponents(sceneRef.current);
    loadComponents(sceneRef.current, components);
    const gridsInitialized = gridsRef.current.gridXY;
    if (!gridsInitialized) {
      const bbox = new THREE.Box3().setFromObject(sceneRef.current);
      const bboxSize = bbox.min.distanceTo(bbox.max);
      setCamPosHome(
        new THREE.Vector3(bboxSize / 2, bboxSize / 2, bboxSize / 2)
      );
      setCamPosSide(new THREE.Vector3(bboxSize / 2, 0, bboxSize / 2));
      setCamPosTop(new THREE.Vector3(0, bboxSize, bboxSize / 2));
      const grids = addGrids(sceneRef.current, bboxSize * 2, gridDivisions);
      gridsRef.current = grids;
    }
    rendererRef.current.render(sceneRef.current, cameraRef.current);
  }, [components]);

  useEffect(() => {
    addRays(sceneRef.current, rays, components);
    rendererRef.current.render(sceneRef.current, cameraRef.current);
  }, [rays]);

  const handleShowRays = async () => {
    setLoading(true);
    if (!showRays || (showRays && !showAllRays)) {
      setRaysInvisible(sceneRef.current);
    } else if (showRays && showAllRays) {
      setPlay(false);
      setRaysVisible(sceneRef.current);
    }
    rendererRef.current.render(sceneRef.current, cameraRef.current);
    setLoading(false);
  };

  const handleShowScatterPoints = async () => {
    setLoading(true);
    if (!showScatterPoints) {
      setScatterPointsInvisible(sceneRef.current);
    } else {
      setScatterPointsVisible(sceneRef.current);
    }
    rendererRef.current.render(sceneRef.current, cameraRef.current);
    setLoading(false);
  };

  useEffect(() => {
    handleShowRays();
  }, [showRays]);

  useEffect(() => {
    handleShowRays();
  }, [showAllRays]);

  useEffect(() => {
    handleShowScatterPoints();
  }, [showScatterPoints]);

  const handleRayChange = async (index, prevIndex) => {
    setLoading(true);
    setRayVisibility(sceneRef.current, prevIndex, false);
    setRayVisibility(sceneRef.current, index, true);
    rendererRef.current.render(sceneRef.current, cameraRef.current);
    setLoading(false);
  };

  useEffect(() => {
    handleRayChange(currentRayIndex, prevRayIndex);
  }, [currentRayIndex]);

  const loop = () => {
    setTimeout(() => {
      handleRayChange(currentRayIndex, prevRayIndex);
      handleNextClick();
      if (playRef.current) {
        loop();
      }
    }, 1000);
  };

  useEffect(() => {
    playRef.current = play;
    if (play) loop();
  }, [play]);

  return (
    <div id="canvas-container" ref={containerRef}>
      <div id="hover-info">{hoverInfo}</div>
    </div>
  );
};

export default ThreeCanvas;
