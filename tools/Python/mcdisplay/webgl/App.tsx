import React from "react";
import ThreeCanvas from "./components/scene/ThreeCanvas";
import ConfigureSceneMenu from "./components/configure-scene/ConfigureSceneMenu";
import "./common.css";
import LoadingIndicator from "./components/loading-indicator/LoadingIndicator";
import { useAppContext } from "./Contexts/AppContext";

export default function App() {
  const { loading, setLoading } = useAppContext();
  return (
    <div id="app" className="column">
      {loading ? <LoadingIndicator /> : null}
      <ConfigureSceneMenu />
      <ThreeCanvas />
    </div>
  );
}
