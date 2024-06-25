import React, { useState } from "react";
import "../../../../common.css";
import "./rays-playback.css";
import { useRaysContext } from "../../../../Contexts/RaysContext";
import Previous from "./previous.svg";
import Next from "./next.svg";
import Play from "./play.svg";
import Pause from "./pause.svg";
import { setRayVisibility } from "../../../../Contexts/addRays";

const RaysPlayback = () => {
  const {
    play,
    setPlay,
    prevRayIndex,
    currentRayIndex,
    setCurrentRayIndex,
    handleNextClick,
    handlePreviousClick,
  } = useRaysContext();
  const { rays, setRays } = useRaysContext();

  const handlePlayPauseClick = () => {
    console.log("handlePlayPauseClick play: ", play);
    setPlay(!play);
  };

  const numRays = rays.rays.length - 1;

  return (
    <div id="rays-playback" className="row">
      <button className="ray-button" onClick={handlePreviousClick}>
        <img src={Previous} alt="Previous" />
      </button>
      <button className="ray-button" onClick={handlePlayPauseClick}>
        {play ? <img src={Pause} alt="Pause" /> : <img src={Play} alt="Play" />}
      </button>
      <button className="ray-button" onClick={handleNextClick}>
        <img src={Next} alt="Next" />
      </button>
      <p className="bold">
        Ray index: {currentRayIndex}/{numRays}
      </p>
    </div>
  );
};

export default RaysPlayback;
