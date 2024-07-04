export declare function useEventCallback<T, K>(handler?: (value: T, event: K) => void): (value: T, event: K) => void;
export declare const isTouch: (event: MouseEvent | TouchEvent) => event is TouchEvent;
export declare const preventDefaultMove: (event: MouseEvent | TouchEvent) => void;
export declare const clamp: (number: number, min?: number, max?: number) => number;
export interface Interaction {
    left: number;
    top: number;
    width: number;
    height: number;
    x: number;
    y: number;
}
export declare const getRelativePosition: (node: HTMLDivElement, event: MouseEvent | TouchEvent) => Interaction;
