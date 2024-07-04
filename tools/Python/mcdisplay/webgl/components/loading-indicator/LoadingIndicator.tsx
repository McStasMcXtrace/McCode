import React from "react";
import "../../common.css";
import "./loading-indicator.css";
import { Watch } from "react-loader-spinner";

const LoadingIndicator = () => {
  return (
    <div id="loading-indicator" className="">
      <Watch
        visible={true}
        height="100"
        width="100"
        radius="48"
        color="#1a73e8"
        ariaLabel="watch-loading"
        wrapperStyle={{}}
        wrapperClass=""
      />
    </div>
  );
};

export default LoadingIndicator;
