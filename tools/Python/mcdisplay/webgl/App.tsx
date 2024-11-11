import React from "react";
import ThreeCanvas from "./components/scene/ThreeCanvas";
import ConfigureSceneMenu from "./components/configure-scene/ConfigureSceneMenu";
import "./common.css";
import LoadingIndicator from "./components/loading-indicator/LoadingIndicator";
import { useAppContext } from "./Contexts/AppContext";
import { initializeInstrument } from "./data-utils/initInstrument";
import { useInstrumentContext } from "./Contexts/InstrumentContext";
import { useRaysContext } from "./Contexts/RaysContext";
import { initializeRays } from "./data-utils/initRays";

export default function App() {
  const { loading, setLoading } = useAppContext();
  const { instrument, setInstrument } = useInstrumentContext();
  const { rays, setRays } = useRaysContext();

  const dropHandler = () => (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    setLoading(true);
    console.log(e.dataTransfer.files);
    const file = e.dataTransfer.files[0];
    const reader = new FileReader();
    reader.onload = (e) => {
      const data = JSON.parse(e.target.result as string);
      //only instrument json contains the key "cmd"
      if (data.hasOwnProperty("cmd")) {
        setInstrument(initializeInstrument(data));
      }
      //only rays json contains the key "numrays"
      else if (data.hasOwnProperty("numrays")) {
        setRays(initializeRays(data));
      }
      setLoading(false);
    };
    reader.readAsText(file);
  };

  const dragOverHandler = () => (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
  };

  return (
    <div
      id="app"
      onDrop={dropHandler()}
      onDragOver={dragOverHandler()}
      className="column"
    >
      {loading ? <LoadingIndicator /> : null}
      <ConfigureSceneMenu />
      <ThreeCanvas />
    </div>
  );
}
