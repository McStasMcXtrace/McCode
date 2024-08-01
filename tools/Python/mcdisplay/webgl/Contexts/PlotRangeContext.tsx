import React, { createContext, useContext, ReactNode, useState } from "react";

type AxisRange = [number, number];

type PlotRangeContextType = {
  plotlyRanges: {
    [key: string]: {
      xaxis: AxisRange;
      yaxis: AxisRange;
    };
  };
  updatePlotlyRanges: (
    view: string,
    newRanges: { xaxis: AxisRange; yaxis: AxisRange }
  ) => void;
};

const PlotRangeContext = createContext<PlotRangeContextType>({
  plotlyRanges: {
    Top: { xaxis: [0, 100], yaxis: [-50, 50] },
    Side: { xaxis: [0, 100], yaxis: [-50, 50] },
    End: { xaxis: [0, 100], yaxis: [-50, 50] },
  },
  updatePlotlyRanges: () => {},
});

interface PlotRangeProviderProps {
  children: ReactNode;
}

export const PlotRangeProvider: React.FC<PlotRangeProviderProps> = ({
  children,
}) => {
  const [plotlyRanges, setPlotlyRanges] = useState({
    Top: { xaxis: [0, 100] as AxisRange, yaxis: [-50, 50] as AxisRange },
    Side: { xaxis: [0, 100] as AxisRange, yaxis: [-50, 50] as AxisRange },
    End: { xaxis: [0, 100] as AxisRange, yaxis: [-50, 50] as AxisRange },
  });

  const updatePlotlyRanges = (
    view: string,
    newRanges: { xaxis: AxisRange; yaxis: AxisRange }
  ) => {
    setPlotlyRanges((prevRanges) => ({
      ...prevRanges,
      [view]: newRanges,
    }));
  };

  return (
    <PlotRangeContext.Provider value={{ plotlyRanges, updatePlotlyRanges }}>
      {children}
    </PlotRangeContext.Provider>
  );
};

export const usePlotRangeContext = () => useContext(PlotRangeContext);
