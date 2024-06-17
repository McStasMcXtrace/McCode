import { Instrument } from "../model/Instrument";

const colors = ['#FF5733', '#33FF57', '#3357FF', '#FF33A8', '#33FFF6'];

export function initializeInstrument(instrument: Instrument): Instrument {
    return {
      ...instrument,
      components: instrument.components.map((component,index) => ({
        id: index,
        name: component.name,
        m4: component.m4,
        drawcalls: component.drawcalls,
        shape: component.drawcalls.key, 
        color: colors[index % colors.length], // Assigning color from the predefined array
        transparency: 1,
      })),
    };
  }