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
  const createOrthographicCamera = (width, height, size) => {
    // Calculate the aspect ratio
    const aspect = width / height;

    // Calculate the boundaries
    const left = -size / 2;
    const right = size / 2;
    const top = size / 2 / aspect;
    const bottom = -(size / 2) / aspect;

    const near = 0.1;
    const far = 1000;
    return new THREE.OrthographicCamera(left, right, top, bottom, near, far);
  };

  // Helper function to assign the correct DOM element
  const getDomElement = (view) => {
    switch (view.view) {
      case "End":
        return backView2DRef;
      case "Top":
        return topView2DRef;
      case "Side":
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
      camera = createOrthographicCamera(width, height, size);
      controls = new OrbitControls(camera, domElement);
      controls.enableRotate = false;
      const cameraHelper = new THREE.CameraHelper(camera);
      view.cameraHelper = cameraHelper;
      //scene.add(cameraHelper);
    } else {
      camera = new THREE.PerspectiveCamera(
        view.fov,
        width / height,
        0.01,
        1000
      );
      controls = new OrbitControls(camera, primaryViewRef);
    }
    controls.mouseButtons = {
      LEFT: THREE.MOUSE.PAN,
      MIDDLE: THREE.MOUSE.DOLLY,
      RIGHT: THREE.MOUSE.ROTATE,
    };
    const position = view.initialCamPos.map((element) => element * size);
    console.log("position: ", position);
    console.log("size: ", size);
    camera.position.fromArray(position);
    camera.up.fromArray(view.up);

    /*panning for orthographic cameras updates plotly axes
    controls.addEventListener('change', () => {
      const xRange = [camera.position.x - 5, camera.position.x + 5];
      const yRange = [camera.position.y - 5, camera.position.y + 5];
      updatePlotlyRanges(view.view, { xaxis: xRange, yaxis: yRange });
    });
  */
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

  const center = new THREE.Vector3(0, 0, 0);

  /* arrow colors should match --x-axis-color, y-ax.. colors in common.css*/
  const x_axis = new THREE.ArrowHelper(
    new THREE.Vector3(1, 0, 0),
    center,
    size,
    0x7f2020,
    1,
    0.5
  );
  const y_axis = new THREE.ArrowHelper(
    new THREE.Vector3(0, 1, 0),
    center,
    size,
    0x207f20,
    1,
    0.5
  );
  const z_axis = new THREE.ArrowHelper(
    new THREE.Vector3(0, 0, 1),
    center,
    size,
    0x20207f,
    1,
    0.5
  );

  axes.x_axis = x_axis;
  axes.y_axis = y_axis;
  axes.z_axis = z_axis;

  scene.add(x_axis);
  scene.add(y_axis);
  scene.add(z_axis);

  return axes;
};

export const addGrids = (scene, gridSize) => {
  /*
  the constants + 5 and -5 are hacks for taking into account that 0,0,0
   is not the true start point of the instrument components may be centered there
    but can extend beyond it.
  */
  const correctedGridSize = gridSize / 2 + 5 || 100;
  //griddivisions is equal to the number of lines in the grid to ensure that each division represents 1 meter.
  const correctedGridDivisions = correctedGridSize || 100;
  const center = gridSize / 4 - 5 || 25;

  const grids = {};
  const gridXZ = new THREE.GridHelper(correctedGridSize, correctedGridSize);
  console.log("correctedGridSize: ", correctedGridSize);
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
