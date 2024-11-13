import React, { useContext } from "react";
import "../../../common.css";
import "./info-view.css";
import { useInstrumentContext } from "../../../Contexts/InstrumentContext";
import { useRaysContext } from "../../../Contexts/RaysContext";
import ExportJSONButton, {
  ExportType,
} from "./export-json-button/ExportJSONButton";
import ImportJson from "./import-json/ImportJson";

const InfoView = () => {
  const { instrument, setInstrument } = useInstrumentContext();
  const { rays, setRays } = useRaysContext();

  return (
    <div id="info-view" className="">
      <h1>{instrument.name}</h1>
      <p>
        <b>Instrument file: </b>
        {instrument.abspath}
      </p>
      <p>
        <b>Number of components: </b>
        {instrument.components.length}
      </p>
      <p>
        <b>Command: </b>
        {instrument.cmd}
      </p>
      {instrument.params.length > 0 ? (
      <p>
        <b>Instrument parameters: </b>
        {instrument.params}
      </p>
      ) : null}
      {instrument.params_defaults.length > 0 ? (
      <p>
        <b>Instrument parameter defaults: </b>
        {instrument.params_defaults}
      </p>
      ) : null}
      {instrument.params_values.length > 0 ? (
      <p>
        <b>Instrument parameters values: </b>
        {instrument.params_values}
      </p>
      ) : null}
      {rays.numrays ? (
        <p>
          <b>Number of rays: </b>
          {rays.numrays}
        </p>
      ) : null}
      {rays.vmax ? (
        <p>
          <b>rays maximum velocity: </b>
          {rays.vmax}
        </p>
      ) : null}
      {rays.vmin ? (
        <p>
          <b>rays minimum velocity: </b>
          {rays.vmin}
        </p>
      ) : null}
      <div className="row">
        <ExportJSONButton
          buttonText="Export Instrument JSON"
          exportType={ExportType.Components}
        />
        {rays.numrays > 0 ? (
          <ExportJSONButton
            buttonText="Export Rays JSON"
            exportType={ExportType.Rays}
          />
        ) : null}
      </div>
      <ImportJson />
    </div>
  );
};

export default InfoView;
