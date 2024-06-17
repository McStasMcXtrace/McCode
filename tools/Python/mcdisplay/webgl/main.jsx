import React, { useState, useEffect, Component } from 'react';
import { createRoot } from 'react-dom/client';
import ThreeCanvas from './components/scene/ThreeCanvas';
import ConfigureSceneMenu from './components/configure-scene/ConfigureSceneMenu';
import './common.css';
import { GridProvider } from './Contexts/GridContext';
import { CameraProvider } from './Contexts/CameraContext';
import { ComponentsProvider } from './Contexts/ComponentsContext';

const App = () => {
    return (
        <div id='app' className='column'>
            <GridProvider>
                <CameraProvider>
                    <ComponentsProvider>
                        <ConfigureSceneMenu/>
                        <ThreeCanvas />
                    </ComponentsProvider>
                </CameraProvider>
            </GridProvider>
        </div>
    );
};

const root = createRoot(document.getElementById('app'));
root.render(<App />);
