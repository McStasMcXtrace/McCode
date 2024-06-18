import React, { ReactNode, createContext, useContext, useState } from "react";
import { Component } from "../model/Component";
import { initializeInstrument } from "../data/initInstrument";
import instrumentdata from "../testdata/instrumentdata.json";

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

export const ComponentsProvider: React.FC<ComponentsProviderProps> = ({children,}) => {
    const [components, setComponents] = useState<Component[]>(
        initializeInstrument(instrumentdata).components
    );

    return (
        <ComponentsContext.Provider value={{ components, setComponents }}>
            {children}
        </ComponentsContext.Provider>
    );
};

export const useComponentsContext = () => useContext(ComponentsContext);
