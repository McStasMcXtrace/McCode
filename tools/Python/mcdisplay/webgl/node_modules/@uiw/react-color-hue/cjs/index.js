"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault")["default"];
Object.defineProperty(exports, "__esModule", {
  value: true
});
exports["default"] = void 0;
var _objectSpread2 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread2"));
var _objectWithoutProperties2 = _interopRequireDefault(require("@babel/runtime/helpers/objectWithoutProperties"));
var _react = _interopRequireDefault(require("react"));
var _reactColorAlpha = _interopRequireDefault(require("@uiw/react-color-alpha"));
var _jsxRuntime = require("react/jsx-runtime");
var _excluded = ["prefixCls", "className", "hue", "onChange", "direction"];
var Hue = /*#__PURE__*/_react["default"].forwardRef(function (props, ref) {
  var _props$prefixCls = props.prefixCls,
    prefixCls = _props$prefixCls === void 0 ? 'w-color-hue' : _props$prefixCls,
    className = props.className,
    _props$hue = props.hue,
    hue = _props$hue === void 0 ? 0 : _props$hue,
    _onChange = props.onChange,
    _props$direction = props.direction,
    direction = _props$direction === void 0 ? 'horizontal' : _props$direction,
    other = (0, _objectWithoutProperties2["default"])(props, _excluded);
  return /*#__PURE__*/(0, _jsxRuntime.jsx)(_reactColorAlpha["default"], (0, _objectSpread2["default"])((0, _objectSpread2["default"])({
    ref: ref,
    className: "".concat(prefixCls, " ").concat(className || '')
  }, other), {}, {
    direction: direction,
    background: "linear-gradient(to ".concat(direction === 'horizontal' ? 'right' : 'bottom', ", rgb(255, 0, 0) 0%, rgb(255, 255, 0) 17%, rgb(0, 255, 0) 33%, rgb(0, 255, 255) 50%, rgb(0, 0, 255) 67%, rgb(255, 0, 255) 83%, rgb(255, 0, 0) 100%)"),
    hsva: {
      h: hue,
      s: 100,
      v: 100,
      a: hue / 360
    },
    onChange: function onChange(_, interaction) {
      _onChange && _onChange({
        h: direction === 'horizontal' ? 360 * interaction.left : 360 * interaction.top
      });
    }
  }));
});
Hue.displayName = 'Hue';
var _default = exports["default"] = Hue;
module.exports = exports.default;