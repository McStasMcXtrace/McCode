"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault")["default"];
var _interopRequireWildcard = require("@babel/runtime/helpers/interopRequireWildcard")["default"];
Object.defineProperty(exports, "__esModule", {
  value: true
});
exports["default"] = void 0;
var _typeof2 = _interopRequireDefault(require("@babel/runtime/helpers/typeof"));
var _objectSpread2 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread2"));
var _objectWithoutProperties2 = _interopRequireDefault(require("@babel/runtime/helpers/objectWithoutProperties"));
var _react = _interopRequireWildcard(require("react"));
var _colorConvert = require("@uiw/color-convert");
var _jsxRuntime = require("react/jsx-runtime");
var _excluded = ["prefixCls", "className", "color", "colors", "style", "rectProps", "onChange", "addonAfter", "addonBefore", "rectRender"];
var Swatch = /*#__PURE__*/_react["default"].forwardRef(function (props, ref) {
  var _props$prefixCls = props.prefixCls,
    prefixCls = _props$prefixCls === void 0 ? 'w-color-swatch' : _props$prefixCls,
    className = props.className,
    color = props.color,
    _props$colors = props.colors,
    colors = _props$colors === void 0 ? [] : _props$colors,
    style = props.style,
    _props$rectProps = props.rectProps,
    rectProps = _props$rectProps === void 0 ? {} : _props$rectProps,
    onChange = props.onChange,
    addonAfter = props.addonAfter,
    addonBefore = props.addonBefore,
    rectRender = props.rectRender,
    other = (0, _objectWithoutProperties2["default"])(props, _excluded);
  var rectStyle = (0, _objectSpread2["default"])({
    '--swatch-background-color': 'rgb(144, 19, 254)',
    background: 'var(--swatch-background-color)',
    height: 15,
    width: 15,
    marginRight: 5,
    marginBottom: 5,
    cursor: 'pointer',
    position: 'relative',
    outline: 'none',
    borderRadius: 2
  }, rectProps.style);
  var handleClick = function handleClick(hex, evn) {
    onChange && onChange((0, _colorConvert.hexToHsva)(hex), (0, _colorConvert.color)((0, _colorConvert.hexToHsva)(hex)), evn);
  };
  return /*#__PURE__*/(0, _jsxRuntime.jsxs)("div", (0, _objectSpread2["default"])((0, _objectSpread2["default"])({
    ref: ref
  }, other), {}, {
    className: [prefixCls, className || ''].filter(Boolean).join(' '),
    style: (0, _objectSpread2["default"])({
      display: 'flex',
      flexWrap: 'wrap',
      position: 'relative'
    }, style),
    children: [addonBefore && /*#__PURE__*/_react["default"].isValidElement(addonBefore) && addonBefore, colors && Array.isArray(colors) && colors.map(function (item, idx) {
      var title = '';
      var background = '';
      if (typeof item === 'string') {
        title = item;
        background = item;
      }
      if ((0, _typeof2["default"])(item) === 'object' && item.color) {
        title = item.title || item.color;
        background = item.color;
      }
      var checked = color && color.toLocaleLowerCase() === background.toLocaleLowerCase();
      var render = rectRender && rectRender({
        title: title,
        color: background,
        checked: !!checked,
        style: (0, _objectSpread2["default"])((0, _objectSpread2["default"])({}, rectStyle), {}, {
          background: background
        }),
        onClick: function onClick(evn) {
          return handleClick(background, evn);
        }
      });
      if (render) {
        return /*#__PURE__*/(0, _jsxRuntime.jsx)(_react.Fragment, {
          children: render
        }, idx);
      }
      var child = rectProps.children && /*#__PURE__*/_react["default"].isValidElement(rectProps.children) ? /*#__PURE__*/_react["default"].cloneElement(rectProps.children, {
        color: background,
        checked: checked
      }) : null;
      return /*#__PURE__*/(0, _jsxRuntime.jsx)("div", (0, _objectSpread2["default"])((0, _objectSpread2["default"])({
        tabIndex: 0,
        title: title,
        onClick: function onClick(evn) {
          return handleClick(background, evn);
        }
      }, rectProps), {}, {
        children: child,
        style: (0, _objectSpread2["default"])((0, _objectSpread2["default"])({}, rectStyle), {}, {
          background: background
        })
      }), idx);
    }), addonAfter && /*#__PURE__*/_react["default"].isValidElement(addonAfter) && addonAfter]
  }));
});
Swatch.displayName = 'Swatch';
var _default = exports["default"] = Swatch;
module.exports = exports.default;