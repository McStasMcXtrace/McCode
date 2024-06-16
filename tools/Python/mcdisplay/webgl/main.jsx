import React, { useState, useEffect } from 'react';
import { createRoot } from 'react-dom/client';
import ThreeCanvas from './components/scene/ThreeCanvas';
import ConfigureSceneMenu from './components/configure-scene/ConfigureSceneMenu';
import './common.css';
import { GridProvider } from './Contexts/GridContext';
import { CameraProvider } from './Contexts/CameraContext';

const App = () => {
    return (
        <div id='app' className='column'>
            <GridProvider>
                <CameraProvider>
                    <ConfigureSceneMenu/>
                    <ThreeCanvas />
                </CameraProvider>
            </GridProvider>
        </div>
    );
};

const root = createRoot(document.getElementById('app'));
root.render(<App />);
