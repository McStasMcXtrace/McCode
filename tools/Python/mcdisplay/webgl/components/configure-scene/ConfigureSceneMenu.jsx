import React from 'react';
import GridButtons from '../grid-buttons/GridButtons'
import ViewButtons from '../view-buttons/ViewButtons';
import './configure-scene-menu.css';

const ConfigureSceneMenu = () => {
    return <div id="configure-scene-menu">
        <h1>Menu</h1>
        <ViewButtons/>
        <GridButtons/>
    </div>;
};

export default ConfigureSceneMenu;
