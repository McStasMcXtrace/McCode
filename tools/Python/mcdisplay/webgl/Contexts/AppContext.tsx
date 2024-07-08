import React, { createContext, useContext, ReactNode, useState } from "react";

type AppContextType = {
  loading: boolean;
  setLoading: React.Dispatch<React.SetStateAction<boolean>>;
  backgroundColor: boolean;
  toggleBackgroundColor: () => void;
};

const AppContext = createContext<AppContextType>({
  loading: true,
  setLoading: () => {},
  backgroundColor: true,
  toggleBackgroundColor: () => {},
});

interface AppProviderProps {
  children: ReactNode;
}

export const AppProvider: React.FC<AppProviderProps> = ({ children }) => {
  const [loading, setLoading] = useState<boolean>(true);
  const [backgroundColor, setBackgroundColor] = useState<boolean>(true);

  const toggleBackgroundColor = () => {
    setBackgroundColor((prevDarkBackground) => !prevDarkBackground);
  };

  return (
    <AppContext.Provider
      value={{ loading, setLoading, backgroundColor, toggleBackgroundColor }}
    >
      {children}
    </AppContext.Provider>
  );
};

export const useAppContext = () => useContext(AppContext);
