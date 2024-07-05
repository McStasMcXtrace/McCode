import React from "react";
import "./common.css";
import { GridProvider } from "./Contexts/GridContext";
import { CameraProvider } from "./Contexts/CameraContext";
import { InstrumentProvider } from "./Contexts/InstrumentContext";
import { RaysProvider } from "./Contexts/RaysContext";
import { AppProvider } from "./Contexts/AppContext";
import App from "./App";
import ReactDOM from "react-dom/client";

ReactDOM.createRoot(document.getElementById("root")!).render(
  <AppProvider>
    <GridProvider>
      <CameraProvider>
        <InstrumentProvider>
          <RaysProvider>
            <App />
          </RaysProvider>
        </InstrumentProvider>
      </CameraProvider>
    </GridProvider>
  </AppProvider>
);
