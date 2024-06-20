import React, { createContext, useContext, useState } from "react";

const GridContext = createContext({
  gridSize: 100,
  gridDivisions: 100,
  setGridSize: (size, divisions) => {},

  showXY: false,
  showXZ: true,
  showYZ: false,
  toggleXY: () => {},
  toggleXZ: () => {},
  toggleYZ: () => {},
});

export const GridProvider = ({ children }) => {
  const [gridSize, setGridSize] = useState(100);
  const [gridDivisions, setGridDivisions] = useState(100);
  const updateGridSize = (size, divisions) => {
    setGridSize(size);
    setGridDivisions(divisions);
  };

  const [showXY, setShowXY] = useState(false);
  const [showXZ, setShowXZ] = useState(true);
  const [showYZ, setShowYZ] = useState(false);

  const toggleXY = () => setShowXY(!showXY);
  const toggleXZ = () => setShowXZ(!showXZ);
  const toggleYZ = () => setShowYZ(!showYZ);

  return (
    <GridContext.Provider
      value={{
        gridSize,
        gridDivisions,
        setGridSize: updateGridSize,
        showXY,
        showXZ,
        showYZ,
        toggleXY,
        toggleXZ,
        toggleYZ,
      }}
    >
      {children}
    </GridContext.Provider>
  );
};

export const useGridContext = () => useContext(GridContext);
