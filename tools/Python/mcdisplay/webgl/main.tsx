import React from "react";
import "./common.css";
import { GridProvider } from "./Contexts/GridContext";
import { CameraProvider } from "./Contexts/CameraContext";
import { InstrumentProvider } from "./Contexts/InstrumentContext";
import { RaysProvider } from "./Contexts/RaysContext";
import { AppProvider } from "./Contexts/AppContext";
import App from "./App";
import ReactDOM from "react-dom/client";
import { PlotRangeProvider } from "./Contexts/PlotRangeContext";

ReactDOM.createRoot(document.getElementById("root")!).render(
  <AppProvider>
    <GridProvider>
      <CameraProvider>
        <InstrumentProvider>
          <RaysProvider>
            <PlotRangeProvider>
              <App />
            </PlotRangeProvider>
          </RaysProvider>
        </InstrumentProvider>
      </CameraProvider>
    </GridProvider>
  </AppProvider>
);
