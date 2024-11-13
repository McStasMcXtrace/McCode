import React, {
  ReactNode,
  createContext,
  useContext,
  useState,
  useEffect,
} from "react";
import { fetchJSON } from "../utils/fetch";
import { initializeInstrument } from "../data-utils/initInstrument";
import { Instrument } from "../model/Instrument";

type InstrumentContextType = {
  instrument: Instrument;
  setInstrument: React.Dispatch<React.SetStateAction<Instrument>>;
};

const InstrumentContext = createContext<InstrumentContextType>({
  instrument: {
    name: "",
    abspath: "",
    params: [],
    params_defaults: [],
    params_values: [],
    cmd: "",
    components: [],
  },
  setInstrument: () => {},
});

interface InstrumentProviderProps {
  children: ReactNode;
}

export const InstrumentProvider: React.FC<InstrumentProviderProps> = ({
  children,
}) => {
  const [instrument, _setInstrument] = useState<Instrument>({
    name: "",
    abspath: "",
    params: [],
    params_defaults: [],
    params_values: [],
    cmd: "",
    components: [],
  });

  useEffect(() => {
    fetchJSON("../instrument.json").then((data) => {
      if (data) {
        const instrument = initializeInstrument(data);
        _setInstrument(instrument);
      } else {
        console.warn("Instrument data is missing");
      }
    });
  }, []);

  const setInstrument = (newInstrument: React.SetStateAction<Instrument>) => {
    _setInstrument(newInstrument);
  };

  return (
    <InstrumentContext.Provider value={{ instrument, setInstrument }}>
      {children}
    </InstrumentContext.Provider>
  );
};

export const useInstrumentContext = () => useContext(InstrumentContext);
