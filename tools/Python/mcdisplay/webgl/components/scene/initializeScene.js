import * as THREE from "three";
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls";

export const initializeScene = () => {
  return new THREE.Scene();
};

export const initializeCameras = (
  width,
  height,
  views,
  size,
  backView2DRef,
  topView2DRef,
  sideView2DRef,
  primaryViewRef
) => {
  // Helper function to create an Orthographic Camera
  const createOrthographicCamera = (width, height) => {
    const left = width / -100;
    const right = width / 100;
    const top = height / 100;
    const bottom = height / -100;
    const size = 1;
    const near = 0.1;
    const far = 1000;
    return new THREE.OrthographicCamera(left, right, top, bottom, near, far);
  };

  // Helper function to assign the correct DOM element
  const getDomElement = (view) => {
    switch (view.view) {
      case "back2D":
        return backView2DRef;
      case "top2D":
        return topView2DRef;
      case "side2D":
        return sideView2DRef;
      default:
        return primaryViewRef;
    }
  };

  views.forEach((view) => {
    let camera;
    let controls;
    const domElement = getDomElement(view);

    if (view.camera === "OrthographicCamera") {
      camera = createOrthographicCamera(width, height);
      controls = new OrbitControls(camera, domElement);
      controls.enableRotate = false;
      const cameraHelper = new THREE.CameraHelper(camera);
      view.cameraHelper = cameraHelper;
      //scene.add(cameraHelper);
    } else {
      camera = new THREE.PerspectiveCamera(view.fov, width / height, 0.01, 1000);
      controls = new OrbitControls(camera, primaryViewRef);
    }

    const position = view.initialCamPos.map((element) => element * size);
    camera.position.fromArray(position);
    camera.up.fromArray(view.up);
    view.controls = controls;
    view.camera = camera;
    view.domElement = domElement;
  });
};

export const initializeRenderer = (width, height) => {
  const renderer = new THREE.WebGLRenderer({
    canvas: document.getElementById("canvas"),
    antialias: true,
  });
  renderer.setPixelRatio(window.devicePixelRatio);
  renderer.setSize(width, height);
  return renderer;
};

export const addAxes = (scene, size) => {

  const axes = {};

  const center = new THREE.Vector3( 0,0,0 );

  /* arrow colors should match --x-axis-color, y-ax.. colors in common.css*/
  const x_axis = new THREE.ArrowHelper( new THREE.Vector3( 1,0,0 ), center, size, 0x7F2020, 1, 0.5 );
  const y_axis = new THREE.ArrowHelper( new THREE.Vector3( 0,1,0 ), center, size, 0x207F20, 1, 0.5 );
  const z_axis = new THREE.ArrowHelper( new THREE.Vector3( 0,0,1 ), center, size, 0x20207F, 1, 0.5 );

  axes.x_axis = x_axis;
  axes.y_axis = y_axis;
  axes.z_axis = z_axis;

  scene.add( x_axis );
  scene.add( y_axis );
  scene.add( z_axis );

  return axes;
};

export const addGrids = (scene, gridSize) => {
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
