import React, { useState, useEffect } from 'react';
import { createRoot } from 'react-dom/client';
import ThreeCanvas from './components/scene/ThreeCanvas';
import ConfigureSceneMenu from './components/configure-scene/ConfigureSceneMenu';
import './common.css';
import { GridProvider } from './Contexts/GridContext';

const App = () => {
    return (
        <div id='app' className='column'>
            <GridProvider>
                <ConfigureSceneMenu/>
                <ThreeCanvas />
            </GridProvider>
        </div>
    );
};

const root = createRoot(document.getElementById('app'));
root.render(<App />);
