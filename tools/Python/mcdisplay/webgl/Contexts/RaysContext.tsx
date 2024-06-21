import React, { ReactNode, createContext, useContext, useState } from "react";
import particledata from "../testdata/particledata.json";
import { RayData } from "../model/Rays";
import { initializeRays } from "../data/initRays";

type RaysContextType = {
  showScatterPoints: boolean;
  toggleScatterPoints: () => void;
  showRays: boolean;
  toggleRays: () => void;
  rays: RayData;
  setRays: React.Dispatch<React.SetStateAction<RayData>>;
};

const RaysContext = createContext<RaysContextType>({
  showScatterPoints: true,
  toggleScatterPoints: () => {},
  showRays: false,
  toggleRays: () => {},
  rays: { rays: [], numrays: 0, vmin: 0, vmax: 0 },
  setRays: () => {},
});

interface RaysProviderProps {
  children: ReactNode;
}

export const RaysProvider: React.FC<RaysProviderProps> = ({ children }) => {
  const [showScatterPoints, setShowScatterPoints] = useState(true);
  const [rays, setRays] = useState<RayData>(initializeRays(particledata));
  const [showRays, setShowRays] = useState(false);

  const toggleScatterPoints = () => {
    setShowScatterPoints((prevShowScatterPoints) => !prevShowScatterPoints);
  };

  const toggleRays = () => {
    setShowRays((prevShowRays) => !prevShowRays);
  };

  return (
    <RaysContext.Provider
      value={{
        showScatterPoints,
        toggleScatterPoints,
        showRays,
        toggleRays,
        rays,
        setRays,
      }}
    >
      {children}
    </RaysContext.Provider>
  );
};

export const useRaysContext = () => useContext(RaysContext);
