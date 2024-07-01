import * as THREE from "three";
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls";
import { element } from "three/examples/jsm/nodes/Nodes.js";

export const initializeScene = () => {
  return new THREE.Scene();
};

export const initializeCameras = (width, height, views, size, frontView2DRef, backView2DRef, topView2DRef, sideView2DRef, primaryViewRef) => {
  views.forEach((view) => {
    let camera;
    let controls;
    let domElement;
    if (view.camera === "OrthographicCamera") {
      const left = width / -20;
      const right = width / 20;
      const top = height / 20;
      const bottom = height  / -20;
      const near = 0.1;
      const far = 1000;
      if(view.view === "back2D"){
        domElement = backView2DRef;
      }else if(view.view === "top2D"){
        domElement = topView2DRef;
      }else if(view.view === "side2D"){
        domElement = sideView2DRef;
      }else if(view.view === "front2D"){
        domElement = frontView2DRef;
      }

      camera = new THREE.OrthographicCamera(left, right, top, bottom, near, far);
      controls = new OrbitControls(camera, domElement);
      controls.enableRotate = false;
    } else {
      camera = new THREE.PerspectiveCamera(
        view.fov,
        width / height,
        0.1,
        1000
      );
      domElement = primaryViewRef;
      controls = new OrbitControls(camera, primaryViewRef);
    }
    const position = view.initialCamPos.map(element => element*size);
    console.log(position)
    camera.position.fromArray(position);
    view.controls = controls;
    view.camera = camera;
    console.log(domElement);
    view.domElement = domElement;
    //const cameraHelper = new THREE.CameraHelper(camera);
    //scene.add(cameraHelper);
  });
};

export const initializeRenderer = (width, height) => {
  const renderer = new THREE.WebGLRenderer({
    canvas: document.getElementById("canvas"),
    antialias: true,
  });
  renderer.setPixelRatio( window.devicePixelRatio );
  renderer.setSize(width, height);
  console.log(width, height);
  return renderer;
};

export const addGrids = (scene, gridSize, gridDivisions) => {
  /*
  the constants + 20 and -10 are hacks for taking into account that 0,0,0
   is not the true start point of the instrument components may be centered there
    but can extend beyond it.
  */
  const correctedGridSize = gridSize / 2 + 5 || 100;
  //griddivisions is equal to the number of lines in the grid to ensure that each division represents 1 meter.
  const correctedGridDivisions = correctedGridSize || 100;
  const center = gridSize / 4 - 5 || 25;

  const grids = {};
  const gridXZ = new THREE.GridHelper(correctedGridSize, correctedGridSize);
  gridXZ.position.set(0, 0, center);
  gridXZ.visible = true;
  gridXZ.name = "gridXZ";
  scene.add(gridXZ);
  grids.gridXZ = gridXZ;

  const gridXY = new THREE.GridHelper(correctedGridSize, correctedGridSize);
  gridXY.position.set(0, 0, center);
  gridXY.visible = false;
  gridXY.rotation.x = Math.PI / 2;
  gridXY.name = "gridXY";
  scene.add(gridXY);
  grids.gridXY = gridXY;

  const gridYZ = new THREE.GridHelper(correctedGridSize, correctedGridSize);
  gridYZ.position.set(0, 0, center);
  gridYZ.visible = false;
  gridYZ.rotation.z = Math.PI / 2;
  gridYZ.name = "gridYZ";
  scene.add(gridYZ);
  grids.gridYZ = gridYZ;

  return grids;
};

export const initializeControls = (camera, renderer) => {
  const controls = new OrbitControls(camera, renderer.domElement);
  controls.update();
  return controls;
};

export const initializeDirectionalLight = (scene) => {
  const light = new THREE.DirectionalLight(0xffffff, 5);
  light.position.set(0, 10, 10);
  scene.add(light);
  return light;
};

export const initializeAmbientLight = (scene) => {
  const light = new THREE.AmbientLight(0x404040);
  scene.add(light);
  return light;
};
