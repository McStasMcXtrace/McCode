"use strict";

var _interopRequireWildcard = require("@babel/runtime/helpers/interopRequireWildcard")["default"];
Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Pointer = void 0;
var _react = _interopRequireWildcard(require("react"));
var _jsxRuntime = require("react/jsx-runtime");
var Pointer = exports.Pointer = function Pointer(_ref) {
  var className = _ref.className,
    color = _ref.color,
    left = _ref.left,
    top = _ref.top,
    prefixCls = _ref.prefixCls;
  var style = {
    position: 'absolute',
    top: top,
    left: left
  };
  var stylePointer = {
    '--saturation-pointer-box-shadow': 'rgb(255 255 255) 0px 0px 0px 1.5px, rgb(0 0 0 / 30%) 0px 0px 1px 1px inset, rgb(0 0 0 / 40%) 0px 0px 1px 2px',
    width: 6,
    height: 6,
    transform: 'translate(-3px, -3px)',
    boxShadow: 'var(--saturation-pointer-box-shadow)',
    borderRadius: '50%',
    backgroundColor: color
  };
  return (0, _react.useMemo)(function () {
    return /*#__PURE__*/(0, _jsxRuntime.jsx)("div", {
      className: "".concat(prefixCls, "-pointer ").concat(className || ''),
      style: style,
      children: /*#__PURE__*/(0, _jsxRuntime.jsx)("div", {
        className: "".concat(prefixCls, "-fill"),
        style: stylePointer
      })
    });
  }, [top, left, color, className, prefixCls]);
};