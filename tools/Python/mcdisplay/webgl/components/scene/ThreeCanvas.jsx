import React, { useEffect, useRef } from 'react';
import { useGridContext } from '../../Contexts/GridContext';
import { useCameraContext } from '../../Contexts/CameraContext';
import './three-canvas.css';
import { initializeScene, initializeCamera, initializeRenderer, initializeGrids, initializeControls } from './utils/initializeScene';

const ThreeCanvas = () => {
    const { showXY, showXZ, showYZ, gridSize, gridDivisions} = useGridContext();
    const {camPos} = useCameraContext();
    const gridsRef = useRef({ gridXY: null, gridXZ: null, gridYZ: null });

    const cameraRef = useRef(null);
    const controlsRef = useRef(null);
    const containerRef = useRef(null);

    useEffect(() => {
      const container = containerRef.current;
      if (!container) return;
      const margin = 20;
      const width = (container.clientWidth || window.innerWidth) - margin;
      const height = (container.clientHeight || window.innerHeight) - margin;

      const scene = initializeScene();
      const camera = initializeCamera(width, height, camPos);
      cameraRef.current = camera;
      const renderer = initializeRenderer(width, height, container);
      const grids = initializeGrids(scene, gridSize, gridDivisions);
      gridsRef.current = grids;
      const controls = initializeControls(camera, renderer);
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

      window.addEventListener('resize', handleResize);

      return () => {
          if (container) {
              container.removeChild(renderer.domElement);
          }
          window.removeEventListener('resize', handleResize);
      };
  }, [camPos, gridSize, gridDivisions]);

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


    return <div id="canvas-container" ref={containerRef}>
    </div>;
};

export default ThreeCanvas;
