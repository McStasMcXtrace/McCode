"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.preventDefaultMove = exports.isTouch = exports.getRelativePosition = exports.clamp = void 0;
exports.useEventCallback = useEventCallback;
var _react = require("react");
// Saves incoming handler to the ref in order to avoid "useCallback hell"
function useEventCallback(handler) {
  var callbackRef = (0, _react.useRef)(handler);
  (0, _react.useEffect)(function () {
    callbackRef.current = handler;
  });
  return (0, _react.useCallback)(function (value, event) {
    return callbackRef.current && callbackRef.current(value, event);
  }, []);
}

// Check if an event was triggered by touch
var isTouch = exports.isTouch = function isTouch(event) {
  return 'touches' in event;
};

// Browsers introduced an intervention, making touch events passive by default.
// This workaround removes `preventDefault` call from the touch handlers.
// https://github.com/facebook/react/issues/19651
var preventDefaultMove = exports.preventDefaultMove = function preventDefaultMove(event) {
  !isTouch(event) && event.preventDefault && event.preventDefault();
};
// Clamps a value between an upper and lower bound.
// We use ternary operators because it makes the minified code
// 2 times shorter then `Math.min(Math.max(a,b),c)`
var clamp = exports.clamp = function clamp(number) {
  var min = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
  var max = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 1;
  return number > max ? max : number < min ? min : number;
};
// Returns a relative position of the pointer inside the node's bounding box
var getRelativePosition = exports.getRelativePosition = function getRelativePosition(node, event) {
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