import React from "react";
import "../../../../common.css";
import "./export-json-button.css";
import { useInstrumentContext } from "../../../../Contexts/InstrumentContext";
import { useRaysContext } from "../../../../Contexts/RaysContext";

export enum ExportType {
  Components = 0,
  Rays = 1,
}

interface ExportJSONButtonProps {
  buttonText: string;
  exportType: ExportType;
}

const ExportJSONButton = ({
  buttonText,
  exportType = ExportType.Components,
}: ExportJSONButtonProps) => {
  const { instrument, setInstrument } = useInstrumentContext();
  const { rays, setRays } = useRaysContext();
  const handleButtonClick = () => {
    let data = {};
    let fileName = "";
    if (exportType === ExportType.Components) {
      data = instrument;
      fileName = instrument.name + "_instrument.json";
    } else if (exportType === ExportType.Rays) {
      data = rays;
      fileName = instrument.name + "_rays.json";
    }
    let element = document.createElement("a");
    element.setAttribute(
      "href",
      "data:text/plain;charset=utf-8," +
        encodeURIComponent(JSON.stringify(data, null, 2))
    );
    element.setAttribute("download", fileName);
    element.style.display = "none";
    document.body.appendChild(element);

    element.click();

    document.body.removeChild(element);
  };

  return (
    <div id="export-json-button" className="">
      <button onClick={handleButtonClick}>{buttonText}</button>
    </div>
  );
};

export default ExportJSONButton;
