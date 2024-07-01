import React, { useState, useEffect, useRef } from "react";
import { useGridContext } from "../../Contexts/GridContext";
import { useCameraContext } from "../../Contexts/CameraContext";
import "./three-canvas.css";
import {
  initializeScene,
  initializeCameras,
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
import { views } from "./utils/views";

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
  let width, height;
  let mouseX = 0,
    mouseY = 0;

  const containerRef = useRef(null);

  const primaryCameraRef = useRef(null);
  const primaryControlsRef = useRef(null);

  const primaryViewRef = useRef(null);
  const TopView2DRef = useRef(null);
  const SideView2DRef = useRef(null);
  const BackView2DRef = useRef(null);
  const FrontView2DRef = useRef(null);

  const rendererRef = useRef(null);
  const sceneRef = useRef(null);
  const playRef = useRef(play);

  function updateSize() {
    let containerWidth, containerHeight;

    if (containerRef.current) {
      containerWidth = containerRef.current.clientWidth;
      containerHeight = containerRef.current.clientHeight;
    }

    const correctWidth = window.innerWidth;
    const correctHeight = window.innerHeight;

    if (width !== correctWidth || height !== correctHeight) {
      width = correctWidth;
      height = correctHeight * 2;

      rendererRef.current.setSize(width, height);
    }
  }

  function render() {
    updateSize();
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
    rendererRef.current.setScissorTest(true);

    if(containerRef.current){
    views.forEach((view) => {
      const aspect = setScissorForElement(view.domElement, rendererRef.current, containerRef.current);

      const camera = view.camera;
      view.updateCamera(camera, sceneRef.current, mouseX, mouseY);
      view.updateControls(view.controls);
      if(camera.type === "PerspectiveCamera"){
        camera.aspect = aspect;
        primaryCameraRef.current = camera;
        primaryControlsRef.current = view.controls;
      }
      camera.updateProjectionMatrix();
      rendererRef.current.render(sceneRef.current, camera);
    });
  }
  }

  function setScissorForElement(elem, renderer, canvas) {
    const canvasRect = canvas.getBoundingClientRect();
    const elemRect = elem.getBoundingClientRect();

    // compute a canvas relative rectangle
    const right = Math.min(elemRect.right, canvasRect.right) - canvasRect.left;
    const left = Math.max(0, elemRect.left - canvasRect.left);
    const bottom = Math.min(elemRect.bottom, canvasRect.bottom) - canvasRect.top;
    const top = Math.max(0, elemRect.top - canvasRect.top);

    const width = Math.min(canvasRect.width, right - left);
    const height = Math.min(canvasRect.height, bottom - top);

    // setup the scissor to only render to that part of the canvas
    const positiveYUpBottom = canvasRect.height - bottom;
    renderer.setScissor(left, positiveYUpBottom, width, height);
    renderer.setViewport(left, positiveYUpBottom, width, height);

    // return the aspect
    return width / height;
  }

  useEffect(() => {
    const container = containerRef.current;
    if (!container) return;
    const width = (container.clientWidth || window.innerWidth) * 2;
    const height = (container.clientHeight || window.innerHeight) * 2;

    const scene = initializeScene();
    sceneRef.current = scene;
    const renderer = initializeRenderer(width, height);
    rendererRef.current = renderer;
    initializeCameras(
      width,
      height,
      views,
      gridSize,
      FrontView2DRef.current,
      BackView2DRef.current,
      TopView2DRef.current,
      SideView2DRef.current,
      primaryViewRef.current
    );

    initializeDirectionalLight(scene);
    initializeAmbientLight(scene);

    function animate() {
      requestAnimationFrame(animate);
      render();
    }

    animate();
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
    if (primaryCameraRef.current) {
      primaryCameraRef.current.position.set(camPos.x, camPos.y, camPos.z);
      if (primaryControlsRef.current) {
        primaryControlsRef.current.target.set(0, 0, camPos.z);
        primaryControlsRef.current.update();
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
    render();
  }, [components]);

  useEffect(() => {
    addRays(sceneRef.current, rays, components);
    render();
  }, [rays]);

  const handleShowRays = async () => {
    setLoading(true);
    if (!showRays || (showRays && !showAllRays)) {
      setRaysInvisible(sceneRef.current);
    } else if (showRays && showAllRays) {
      setPlay(false);
      setRaysVisible(sceneRef.current);
    }
    render();
    setLoading(false);
  };

  const handleShowScatterPoints = async () => {
    setLoading(true);
    if (!showScatterPoints) {
      setScatterPointsInvisible(sceneRef.current);
    } else {
      setScatterPointsVisible(sceneRef.current);
    }
    render();
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
    render();
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
      <canvas id="canvas"></canvas>
      <div id="views">
        <div className="view" id="primaryView" ref={primaryViewRef}>3D</div>
        <div className="column fill row-gap two-D">
          <div className="row fill">
            <div className="view" id="TopView2D" ref={TopView2DRef}>Top</div>
            <div className="view" id="SideView2D" ref={SideView2DRef}>Side</div>
          </div>
          <div className="row fill">
            <div className="view" id="BackView2D" ref={BackView2DRef}>Back</div>
            <div className="view" id="FrontView2D" ref={FrontView2DRef}>Front</div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ThreeCanvas;
