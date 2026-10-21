import { Instrument } from "../model/Instrument";

const colors = [
  "#F80000",
  "#00F800",
  "#0000F8",
  "#00F8F8",
  "#F800F8",
  "#00F880",
  "#F8F800",
  "#F88000",
  "#80F800",
  "#0080F8",
  "#8000F8",
  "#F80080",
  "#A8A8A8",
];

// hollow box/cylinder (thickness > 0): translucent by default, so the closed inner cavity shows
const HOLLOW_TRANSPARENCY = 0.4;
const isHollow = (drawcall: any): boolean =>
  (drawcall.key === "box" && drawcall.args[6] > 0) ||
  (drawcall.key === "cylinder" && drawcall.args[5] > 0);

export function initializeInstrument(data: any): Instrument {
  return {
    ...data,
    components: data.components.map((component, index) => ({
      id: index,
      name: component.name,
      m4: component.m4,
      drawcalls: component.drawcalls,
      color: colors[index % colors.length], // Assigning color from the colors array
      transparency: component.drawcalls.some(isHollow) ? HOLLOW_TRANSPARENCY : 1,
    })),
  };
}
