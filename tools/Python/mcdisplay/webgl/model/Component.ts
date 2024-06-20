export interface Component {
  id: number;
  name: string;
  m4: number[];
  drawcalls: DrawCall;
  shape: string;
  color: string;
  transparency: number;
}

interface DrawCall {
  key: string;
  args: any[];
}
