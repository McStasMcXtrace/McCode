import React, { createContext, useContext, useState } from 'react';
import * as THREE from 'three';

const ComponentsContext = createContext({
    components: [],
    setComponents: () => {},
});

export const ComponentsProvider = ({ children }) => {
    const [components, setComponents] = useState([new THREE.Mesh(new THREE.SphereGeometry(1, 32, 32), new THREE.MeshLambertMaterial({ color: 0xffff00 })), new THREE.Mesh(new THREE.SphereGeometry(1, 32, 32), new THREE.MeshLambertMaterial({ color: 0xff0000 }))]);

    return (
        <ComponentsContext.Provider value={{ components, setComponents}}>
            {children}
        </ComponentsContext.Provider>
    );
}

export const useComponentsContext = () => useContext(ComponentsContext);
