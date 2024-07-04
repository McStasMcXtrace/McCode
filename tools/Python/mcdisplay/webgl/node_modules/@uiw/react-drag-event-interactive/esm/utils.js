import { useRef, useEffect, useCallback } from 'react';

// Saves incoming handler to the ref in order to avoid "useCallback hell"
export function useEventCallback(handler) {
  var callbackRef = useRef(handler);
  useEffect(() => {
    callbackRef.current = handler;
  });
  return useCallback((value, event) => callbackRef.current && callbackRef.current(value, event), []);
}

// Check if an event was triggered by touch
export var isTouch = event => 'touches' in event;

// Browsers introduced an intervention, making touch events passive by default.
// This workaround removes `preventDefault` call from the touch handlers.
// https://github.com/facebook/react/issues/19651
export var preventDefaultMove = event => {
  !isTouch(event) && event.preventDefault && event.preventDefault();
};
// Clamps a value between an upper and lower bound.
// We use ternary operators because it makes the minified code
// 2 times shorter then `Math.min(Math.max(a,b),c)`
export var clamp = function clamp(number, min, max) {
  if (min === void 0) {
    min = 0;
  }
  if (max === void 0) {
    max = 1;
  }
  return number > max ? max : number < min ? min : number;
};
// Returns a relative position of the pointer inside the node's bounding box
export var getRelativePosition = (node, event) => {
  var rect = node.getBoundingClientRect();

  // Get user's pointer position from `touches` array if it's a `TouchEvent`
  var pointer = isTouch(event) ? event.touches[0] : event;
  return {
    left: clamp((pointer.pageX - (rect.left + window.pageXOffset)) / rect.width),
    top: clamp((pointer.pageY - (rect.top + window.pageYOffset)) / rect.height),
    width: rect.width,
    height: rect.height,
    x: pointer.pageX - (rect.left + window.pageXOffset),
    y: pointer.pageY - (rect.top + window.pageYOffset)
  };
};