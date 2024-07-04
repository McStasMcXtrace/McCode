"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault")["default"];
Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.Pointer = void 0;
var _objectSpread2 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread2"));
var _objectWithoutProperties2 = _interopRequireDefault(require("@babel/runtime/helpers/objectWithoutProperties"));
var _react = _interopRequireDefault(require("react"));
var _jsxRuntime = require("react/jsx-runtime");
var _excluded = ["className", "prefixCls", "left", "top", "style", "fillProps"];
var Pointer = exports.Pointer = function Pointer(_ref) {
  var className = _ref.className,
    prefixCls = _ref.prefixCls,
    left = _ref.left,
    top = _ref.top,
    style = _ref.style,
    fillProps = _ref.fillProps,
    reset = (0, _objectWithoutProperties2["default"])(_ref, _excluded);
  var styleWrapper = (0, _objectSpread2["default"])((0, _objectSpread2["default"])({}, style), {}, {
    position: 'absolute',
    left: left,
    top: top
  });
  var stylePointer = (0, _objectSpread2["default"])((0, _objectSpread2["default"])({
    width: 18,
    height: 18,
    boxShadow: 'var(--alpha-pointer-box-shadow)',
    borderRadius: '50%',
    backgroundColor: 'var(--alpha-pointer-background-color)'
  }, fillProps === null || fillProps === void 0 ? void 0 : fillProps.style), {}, {
    transform: left ? 'translate(-9px, -1px)' : 'translate(-1px, -9px)'
  });
  return /*#__PURE__*/(0, _jsxRuntime.jsx)("div", (0, _objectSpread2["default"])((0, _objectSpread2["default"])({
    className: "".concat(prefixCls, "-pointer ").concat(className || ''),
    style: styleWrapper
  }, reset), {}, {
    children: /*#__PURE__*/(0, _jsxRuntime.jsx)("div", (0, _objectSpread2["default"])((0, _objectSpread2["default"])({
      className: "".concat(prefixCls, "-fill")
    }, fillProps), {}, {
      style: stylePointer
    }))
  }));
};