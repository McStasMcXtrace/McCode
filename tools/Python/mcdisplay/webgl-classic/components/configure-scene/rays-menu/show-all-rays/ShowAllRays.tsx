import React from "react";
import "../../../../common.css";
import "./show-all-rays.css";
import { useRaysContext } from "../../../../Contexts/RaysContext";

interface ShowAllRaysProps {
  text: string;
}

const ShowAllRays = ({ text }: ShowAllRaysProps) => {
  const { showAllRays, toggleShowAllRays } = useRaysContext();

  return (
    <div id="show-all-rays" className="">
      <button onClick={toggleShowAllRays}>{text}</button>
    </div>
  );
};

export default ShowAllRays;
