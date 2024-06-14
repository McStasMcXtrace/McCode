import React, { useEffect, useRef } from 'react';
import  * as THREE from 'three';
import { useGridContext } from '../../Contexts/GridContext';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls';
import './three-canvas.css';

const ThreeCanvas = () => {
    const { showXY, showXZ, showYZ} = useGridContext();
    const gridsRef = useRef({ gridXY: null, gridXZ: null, gridYZ: null });

    const containerRef = useRef(null);

    useEffect(() => {
    const container = containerRef.current;
    if (!container) return;
    const margin = 20;
    const width = (container.clientWidth || window.innerWidth) - margin * 2;
    const height = (container.clientHeight || window.innerHeight)-margin * 3.5;
    console.log(width, height);
    const scene = new THREE.Scene();
    const camera = new THREE.PerspectiveCamera(75, width / height, 0.1, 1000);
    camera.position.set(10, 20, 30); // Set a default camera position
    const renderer = new THREE.WebGLRenderer();
    renderer.setSize(width, height);

    if (containerRef.current) {
        containerRef.current.appendChild(renderer.domElement);
    }

    // Create grids
    const gridXZ = new THREE.GridHelper(100, 100);
    gridXZ.name = "gridXZ";
    scene.add(gridXZ);
    gridsRef.current.gridXZ = gridXZ;

    const gridXY = new THREE.GridHelper(100, 100);
    gridXY.rotation.x = Math.PI / 2;
    gridXY.name = "gridXY";
    scene.add(gridXY);
    gridsRef.current.gridXY = gridXY;

    const gridYZ = new THREE.GridHelper(100, 100);
    gridYZ.rotation.z = Math.PI / 2;
    gridYZ.name = "gridYZ";
    scene.add(gridYZ);
    gridsRef.current.gridYZ = gridYZ;

    // Add OrbitControls
    const controls = new OrbitControls(camera, renderer.domElement);
    controls.update();

    function animate() {
        requestAnimationFrame(animate);
        renderer.render(scene, camera);
    }

    animate();

    const handleResize = () => {
      const newWidth = container.clientWidth - margin * 2;
      const newHeight = container.clientHeight - margin * 2;
      camera.aspect = newWidth / newHeight;
      camera.updateProjectionMatrix();
      renderer.setSize(newWidth, newHeight);
    };

    window.addEventListener('resize', handleResize);

    return () => {
        if (containerRef.current) {
          containerRef.current.removeChild(renderer.domElement);
        }
        window.removeEventListener('resize', handleResize);
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


    return <div id="canvas-container" ref={containerRef}>
    </div>;
};

export default ThreeCanvas;
