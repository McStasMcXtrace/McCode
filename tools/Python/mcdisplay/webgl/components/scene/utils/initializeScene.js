import * as THREE from "three";
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls";

export const initializeScene = () => {
  return new THREE.Scene();
};

export const initializeCamera = (width, height, camPos) => {
  const camera = new THREE.PerspectiveCamera(75, width / height, 0.1, 1000);
  camera.position.set(camPos.x, camPos.y, camPos.z);
  return camera;
};

export const initializeRenderer = (width, height, container) => {
  const renderer = new THREE.WebGLRenderer({
    antialias: true,
  });
  renderer.setSize(width, height);
  if (container) {
    container.appendChild(renderer.domElement);
  }
  return renderer;
};

export const initializeGrids = (scene, gridSize, gridDivisions) => {
  const grids = {};
  const gridXZ = new THREE.GridHelper(gridSize, gridDivisions);
  gridXZ.visible = true;
  gridXZ.name = "gridXZ";
  scene.add(gridXZ);
  grids.gridXZ = gridXZ;

  const gridXY = new THREE.GridHelper(gridSize, gridDivisions);
  gridXY.visible = false;
  gridXY.rotation.x = Math.PI / 2;
  gridXY.name = "gridXY";
  scene.add(gridXY);
  grids.gridXY = gridXY;

  const gridYZ = new THREE.GridHelper(gridSize, gridDivisions);
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
