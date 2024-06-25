import React, { ReactNode, createContext, useContext, useState } from "react";
import particledata from "../testdata/particledata.json";
import { RayData } from "../model/Rays";
import { initializeRays } from "../data/initRays";

type RaysContextType = {
  play: boolean;
  setPlay: React.Dispatch<React.SetStateAction<boolean>>;
  prevRayIndex: number;
  currentRayIndex: number;
  setCurrentRayIndex: (index: number) => void;
  showScatterPoints: boolean;
  toggleScatterPoints: () => void;
  showRays: boolean;
  toggleRays: () => void;
  rays: RayData;
  setRays: React.Dispatch<React.SetStateAction<RayData>>;
  handleNextClick: () => void;
  handlePreviousClick: () => void;
};

const RaysContext = createContext<RaysContextType>({
  play: false,
  setPlay: () => {},
  prevRayIndex: 0,
  currentRayIndex: 0,
  setCurrentRayIndex: () => {},
  showScatterPoints: true,
  toggleScatterPoints: () => {},
  showRays: false,
  toggleRays: () => {},
  rays: { rays: [], numrays: 0, vmin: 0, vmax: 0 },
  setRays: () => {},
  handleNextClick: () => {},
  handlePreviousClick: () => {},
});

interface RaysProviderProps {
  children: ReactNode;
}

export const RaysProvider: React.FC<RaysProviderProps> = ({ children }) => {
  const [showScatterPoints, setShowScatterPoints] = useState(true);
  const [rays, setRays] = useState<RayData>(initializeRays(particledata));
  const [showRays, setShowRays] = useState(false);
  const [currentRayIndex, _setCurrentRayIndex] = useState(0);
  const [prevRayIndex, setPrevRayIndex] = useState(0);
  const [play, setPlay] = useState(false);

  const setCurrentRayIndex = (
    indexOrUpdater: number | ((prevIndex: number) => number)
  ) => {
    if (typeof indexOrUpdater === "function") {
      _setCurrentRayIndex((prevIndex) => {
        const newIndex = indexOrUpdater(prevIndex);
        setPrevRayIndex(prevIndex);
        return newIndex;
      });
    } else {
      setPrevRayIndex(currentRayIndex);
      _setCurrentRayIndex(indexOrUpdater);
    }
  };

  const toggleScatterPoints = () => {
    setShowScatterPoints((prevShowScatterPoints) => !prevShowScatterPoints);
  };

  const toggleRays = () => {
    setShowRays((prevShowRays) => !prevShowRays);
  };

  const handleNextClick = () => {
    setCurrentRayIndex((prevIndex) => {
      const numRays = rays.rays.length - 1;
      if (prevIndex >= numRays) {
        return 0; // Start from the beginning
      } else {
        return prevIndex + 1;
      }
    });
  };

  const handlePreviousClick = () => {
    setCurrentRayIndex((prevIndex) => {
      const numRays = rays.rays.length - 1;
      if (prevIndex <= 0) {
        return numRays; // Go to the last ray
      } else {
        return prevIndex - 1;
      }
    });
  };

  return (
    <RaysContext.Provider
      value={{
        play,
        setPlay,
        prevRayIndex,
        currentRayIndex,
        setCurrentRayIndex,
        showScatterPoints,
        toggleScatterPoints,
        showRays,
        toggleRays,
        rays,
        setRays,
        handleNextClick,
        handlePreviousClick,
      }}
    >
      {children}
    </RaysContext.Provider>
  );
};

export const useRaysContext = () => useContext(RaysContext);
