import React, { createContext, useContext, useState } from 'react';

const GridContext = createContext({
    showXY: false,
    showXZ: true,
    showYZ: false,
    toggleXY: () => {},
    toggleXZ: () => {},
    toggleYZ: () => {},
});

export const GridProvider = ({ children }) => {
    const [showXY, setShowXY] = useState(false);
    const [showXZ, setShowXZ] = useState(true);
    const [showYZ, setShowYZ] = useState(false);

    const toggleXY = () => setShowXY(!showXY);
    const toggleXZ = () => setShowXZ(!showXZ);
    const toggleYZ = () => setShowYZ(!showYZ);

    return (
        <GridContext.Provider value={{ showXY, showXZ, showYZ, toggleXY, toggleXZ, toggleYZ }}>
            {children}
        </GridContext.Provider>
    );
}

export const useGridContext = () => useContext(GridContext);