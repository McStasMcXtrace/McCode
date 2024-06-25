import React, { createContext, useContext, ReactNode, useState } from "react";

type AppContextType = {
  loading: boolean;
  setLoading: React.Dispatch<React.SetStateAction<boolean>>;
};

const AppContext = createContext<AppContextType>({
  loading: true,
  setLoading: () => {},
});

interface AppProviderProps {
  children: ReactNode;
}

export const AppProvider: React.FC<AppProviderProps> = ({ children }) => {
  const [loading, _setLoading] = useState<boolean>(true);
  const setLoading = (value: React.SetStateAction<boolean>) => {
    _setLoading(value);
  };
  return (
    <AppContext.Provider value={{ loading, setLoading }}>
      {children}
    </AppContext.Provider>
  );
};

export const useAppContext = () => useContext(AppContext);
