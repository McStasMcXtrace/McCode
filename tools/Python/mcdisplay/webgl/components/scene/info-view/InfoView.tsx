import React, { useContext } from "react";
import "../../../common.css";
import "./info-view.css";
import { useInstrumentContext } from "../../../Contexts/InstrumentContext";
import { useRaysContext } from "../../../Contexts/RaysContext";

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
      <p>
        <b>Instrument parameters: </b>
        {instrument.params}
      </p>
      <p>
        <b>Instrument parameter defaults: </b>
        {instrument.params_defaults}
      </p>
      <p>
        <b>Instrument parameters values: </b>
        {instrument.params_values}
      </p>
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
    </div>
  );
};

export default InfoView;
