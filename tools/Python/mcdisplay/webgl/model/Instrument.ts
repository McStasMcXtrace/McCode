import { Component } from "./Component";

export interface Instrument {
  name: string;
  abspath: string;
  params: any[];
  params_defaults: any[];
  params_values: any[];
  cmd: string;
  components: Component[];
}
