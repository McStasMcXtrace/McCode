import React from "react";
import ThreeCanvas from "./components/scene/ThreeCanvas";
import ConfigureSceneMenu from "./components/configure-scene/ConfigureSceneMenu";
import "./common.css";
import LoadingIndicator from "./components/loading-indicator/LoadingIndicator";
import { useAppContext } from "./Contexts/AppContext";
import { initializeInstrument } from "./data-utils/initInstrument";
import { useInstrumentContext } from "./Contexts/InstrumentContext";

export default function App() {
  const { loading, setLoading } = useAppContext();
  const { instrument, setInstrument } = useInstrumentContext();

  const dropHandler = () => (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    setLoading(true);
    console.log(e.dataTransfer.files);
    const file = e.dataTransfer.files[0];
    const reader = new FileReader();
    reader.onload = (e) => {
      const data = JSON.parse(e.target.result as string);
      const newInstrument = instrument;
      newInstrument.components = data;
      setInstrument(initializeInstrument(newInstrument));
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
