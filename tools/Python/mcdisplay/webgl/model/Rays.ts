export interface Args {
  args: number[];
}

export interface Group {
  compname: string;
  events: Args[];
}

export interface Ray {
  groups: Group[];
  speed: number;
}

export interface RayData {
  rays: Ray[];
  numrays: number;
  vmin: number;
  vmax: number;
}
