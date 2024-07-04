import React, {
  ReactNode,
  createContext,
  useContext,
  useState,
  useEffect,
} from "react";
import { Component } from "../model/Component";
import { fetchJSON } from "../utils/fetch";
import { initializeInstrument } from "../data-utils/initInstrument";

type ComponentsContextType = {
  components: Component[];
  setComponents: React.Dispatch<React.SetStateAction<Component[]>>;
};

const ComponentsContext = createContext<ComponentsContextType>({
  components: [],
  setComponents: () => {},
});

interface ComponentsProviderProps {
  children: ReactNode;
}

export const ComponentsProvider: React.FC<ComponentsProviderProps> = ({
  children,
}) => {
  const [components, _setComponents] = useState<Component[]>([]);

  useEffect(() => {
    fetchJSON("../instrument.json").then((data) => {
      if (data) {
        const instrument = initializeInstrument(data);
        _setComponents(instrument.components);
      } else {
        console.warn("Instrument data is missing");
      }
    });
  }, []);

  const setComponents = (newComponents: React.SetStateAction<Component[]>) => {
    _setComponents(newComponents);
  };

  return (
    <ComponentsContext.Provider value={{ components, setComponents }}>
      {children}
    </ComponentsContext.Provider>
  );
};

export const useComponentsContext = () => useContext(ComponentsContext);
