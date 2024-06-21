import React, { ReactNode, createContext, useContext, useState } from "react";
import particledata from "../testdata/particledata.json";
import { RayData } from "../model/Rays";
import { initializeRays } from "../data/initRays";

type RaysContextType = {
  showRays: boolean;
  toggleRays: () => void;
  rays: RayData;
  setRays: React.Dispatch<React.SetStateAction<RayData>>;
};

const RaysContext = createContext<RaysContextType>({
  showRays: false,
  toggleRays: () => {},
  rays: { rays: [], numrays: 0, vmin: 0, vmax: 0 },
  setRays: () => {},
});

interface RaysProviderProps {
  children: ReactNode;
}

export const RaysProvider: React.FC<RaysProviderProps> = ({ children }) => {
  const [rays, setRays] = useState<RayData>(initializeRays(particledata));
  const [showRays, setShowRays] = useState(false);

  const toggleRays = () => {
    setShowRays((prevShowRays) => !prevShowRays);
  };

  return (
    <RaysContext.Provider value={{ showRays, toggleRays, rays, setRays }}>
      {children}
    </RaysContext.Provider>
  );
};

export const useRaysContext = () => useContext(RaysContext);
