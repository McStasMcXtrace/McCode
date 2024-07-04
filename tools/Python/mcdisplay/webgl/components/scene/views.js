import { label } from "three/examples/jsm/nodes/Nodes.js";

export const views = [
  // Main view Perspective camera
  {
    left: 0.0,
    bottom: 0.5,
    width: 1.0,
    height: 0.5,
    camera: "PerspectiveCamera",
    view: "primary",
    initialCamPos: [1, 1, 0.1],
    up: [0, 1, 0],
    fov: 60,
    updateCamera: function (camera, scene, mouseX) {
      camera.position.x += mouseX * 0.05;
      camera.position.x = Math.max(Math.min(camera.position.x, 2000), -2000);
      camera.lookAt(scene.position);
    },
    updateControls: function (controls) {
      controls.update();
    },
  },
  // PyQtGraph replacements using Ortographic cameras
  // Top view - XZ
  {
    camera: "OrthographicCamera",
    view: "top2D",
    initialCamPos: [0, 1, 0],
    up: [1, 0, 0],
    x_label: "z",
    y_label: "x",
    updateCamera: function (camera, scene, mouseX) {
      camera.position.x += mouseX * 0.05;
      camera.position.x = Math.max(Math.min(camera.position.x, 2000), -2000);
      camera.lookAt(scene.position);
    },
    updateControls: function (controls) {
      controls.update();
    },
  },
  // Back view - XY
  {
    camera: "OrthographicCamera",
    view: "back2D",
    initialCamPos: [0, 0, 1],
    up: [0, 1, 0],
    x_label: "x",
    y_label: "y",
    updateCamera: function (camera, scene, mouseX) {
      camera.position.x = Math.max(Math.min(camera.position.x, 2000), -2000);
      camera.lookAt(scene.position);
    },
    updateControls: function (controls) {
      controls.update();
    },
  },
  //Side view - YZ
  {
    camera: "OrthographicCamera",
    view: "side2D",
    initialCamPos: [1, 0, 0],
    up: [0, 1, 0],
    x_label: "z",
    y_label: "y",
    updateCamera: function (camera, scene, mouseX) {
      camera.position.x += mouseX * 0.05;
      camera.position.x = Math.max(Math.min(camera.position.x, 2000), -2000);
      camera.lookAt(scene.position);
    },
    updateControls: function (controls) {
      controls.update();
    },
  },
];
