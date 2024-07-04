"use strict";

var _interopRequireDefault = require("@babel/runtime/helpers/interopRequireDefault")["default"];
var _interopRequireWildcard = require("@babel/runtime/helpers/interopRequireWildcard")["default"];
Object.defineProperty(exports, "__esModule", {
  value: true
});
exports["default"] = void 0;
var _objectSpread2 = _interopRequireDefault(require("@babel/runtime/helpers/objectSpread2"));
var _typeof2 = _interopRequireDefault(require("@babel/runtime/helpers/typeof"));
var _slicedToArray2 = _interopRequireDefault(require("@babel/runtime/helpers/slicedToArray"));
var _objectWithoutProperties2 = _interopRequireDefault(require("@babel/runtime/helpers/objectWithoutProperties"));
var _react = _interopRequireWildcard(require("react"));
var _reactColorSaturation = _interopRequireDefault(require("@uiw/react-color-saturation"));
var _reactColorAlpha = _interopRequireDefault(require("@uiw/react-color-alpha"));
var _reactColorEditableInput = _interopRequireDefault(require("@uiw/react-color-editable-input"));
var _reactColorEditableInputRgba = _interopRequireDefault(require("@uiw/react-color-editable-input-rgba"));
var _reactColorHue = _interopRequireDefault(require("@uiw/react-color-hue"));
var _colorConvert = require("@uiw/color-convert");
var _reactColorSwatch = _interopRequireDefault(require("@uiw/react-color-swatch"));
var _jsxRuntime = require("react/jsx-runtime");
var _excluded = ["prefixCls", "className", "onChange", "width", "presetColors", "color", "editableDisable", "disableAlpha", "style"];
var PRESET_COLORS = ['#D0021B', '#F5A623', '#f8e61b', '#8B572A', '#7ED321', '#417505', '#BD10E0', '#9013FE', '#4A90E2', '#50E3C2', '#B8E986', '#000000', '#4A4A4A', '#9B9B9B', '#FFFFFF'];
var Bar = function Bar(props) {
  return /*#__PURE__*/(0, _jsxRuntime.jsx)("div", {
    style: {
      boxShadow: 'rgb(0 0 0 / 60%) 0px 0px 2px',
      width: 4,
      top: 1,
      bottom: 1,
      left: props.left,
      borderRadius: 1,
      position: 'absolute',
      backgroundColor: '#fff'
    }
  });
};
var Sketch = /*#__PURE__*/_react["default"].forwardRef(function (props, ref) {
  var _props$prefixCls = props.prefixCls,
    prefixCls = _props$prefixCls === void 0 ? 'w-color-sketch' : _props$prefixCls,
    className = props.className,
    onChange = props.onChange,
    _props$width = props.width,
    width = _props$width === void 0 ? 218 : _props$width,
    _props$presetColors = props.presetColors,
    presetColors = _props$presetColors === void 0 ? PRESET_COLORS : _props$presetColors,
    color = props.color,
    _props$editableDisabl = props.editableDisable,
    editableDisable = _props$editableDisabl === void 0 ? true : _props$editableDisabl,
    _props$disableAlpha = props.disableAlpha,
    disableAlpha = _props$disableAlpha === void 0 ? false : _props$disableAlpha,
    style = props.style,
    other = (0, _objectWithoutProperties2["default"])(props, _excluded);
  var _useState = (0, _react.useState)({
      h: 209,
      s: 36,
      v: 90,
      a: 1
    }),
    _useState2 = (0, _slicedToArray2["default"])(_useState, 2),
    hsva = _useState2[0],
    setHsva = _useState2[1];
  (0, _react.useEffect)(function () {
    if (typeof color === 'string' && (0, _colorConvert.validHex)(color)) {
      setHsva((0, _colorConvert.hexToHsva)(color));
    }
    if ((0, _typeof2["default"])(color) === 'object') {
      setHsva(color);
    }
  }, [color]);
  var handleChange = function handleChange(hsv) {
    setHsva(hsv);
    onChange && onChange((0, _colorConvert.color)(hsv));
  };
  var handleHex = function handleHex(value, evn) {
    if (typeof value === 'string' && (0, _colorConvert.validHex)(value) && /(3|6)/.test(String(value.length))) {
      handleChange((0, _colorConvert.hexToHsva)(value));
    }
  };
  var handleAlphaChange = function handleAlphaChange(newAlpha) {
    return handleChange((0, _objectSpread2["default"])((0, _objectSpread2["default"])({}, hsva), {
      a: newAlpha.a
    }));
  };
  var handleSaturationChange = function handleSaturationChange(newColor) {
    return handleChange((0, _objectSpread2["default"])((0, _objectSpread2["default"])((0, _objectSpread2["default"])({}, hsva), newColor), {}, {
      a: hsva.a
    }));
  };
  var styleMain = (0, _objectSpread2["default"])({
    '--sketch-background': 'rgb(255, 255, 255)',
    '--sketch-box-shadow': 'rgb(0 0 0 / 15%) 0px 0px 0px 1px, rgb(0 0 0 / 15%) 0px 8px 16px',
    '--sketch-swatch-box-shadow': 'rgb(0 0 0 / 15%) 0px 0px 0px 1px inset',
    '--sketch-alpha-box-shadow': 'rgb(0 0 0 / 15%) 0px 0px 0px 1px inset, rgb(0 0 0 / 25%) 0px 0px 4px inset',
    '--sketch-swatch-border-top': '1px solid rgb(238, 238, 238)',
    background: 'var(--sketch-background)',
    borderRadius: 4,
    boxShadow: 'var(--sketch-box-shadow)',
    width: width
  }, style);
  var styleAlpha = {
    borderRadius: 2,
    background: (0, _colorConvert.hsvaToRgbaString)(hsva),
    boxShadow: 'var(--sketch-alpha-box-shadow)'
  };
  var styleSwatch = {
    borderTop: 'var(--sketch-swatch-border-top)',
    paddingTop: 10,
    paddingLeft: 10
  };
  var styleSwatchRect = {
    marginRight: 10,
    marginBottom: 10,
    borderRadius: 3,
    boxShadow: 'var(--sketch-swatch-box-shadow)'
  };
  return /*#__PURE__*/(0, _jsxRuntime.jsxs)("div", (0, _objectSpread2["default"])((0, _objectSpread2["default"])({}, other), {}, {
    className: "".concat(prefixCls, " ").concat(className || ''),
    ref: ref,
    style: styleMain,
    children: [/*#__PURE__*/(0, _jsxRuntime.jsxs)("div", {
      style: {
        padding: '10px 10px 8px'
      },
      children: [/*#__PURE__*/(0, _jsxRuntime.jsx)(_reactColorSaturation["default"], {
        hsva: hsva,
        style: {
          width: 'auto',
          height: 150
        },
        onChange: handleSaturationChange
      }), /*#__PURE__*/(0, _jsxRuntime.jsxs)("div", {
        style: {
          display: 'flex',
          marginTop: 4
        },
        children: [/*#__PURE__*/(0, _jsxRuntime.jsxs)("div", {
          style: {
            flex: 1
          },
          children: [/*#__PURE__*/(0, _jsxRuntime.jsx)(_reactColorHue["default"], {
            width: "auto",
            height: 10,
            hue: hsva.h,
            pointer: Bar,
            innerProps: {
              style: {
                marginLeft: 1,
                marginRight: 5
              }
            },
            onChange: function onChange(newHue) {
              return handleChange((0, _objectSpread2["default"])((0, _objectSpread2["default"])({}, hsva), newHue));
            }
          }), !disableAlpha && /*#__PURE__*/(0, _jsxRuntime.jsx)(_reactColorAlpha["default"], {
            width: "auto",
            height: 10,
            hsva: hsva,
            pointer: Bar,
            style: {
              marginTop: 4
            },
            innerProps: {
              style: {
                marginLeft: 1,
                marginRight: 5
              }
            },
            onChange: handleAlphaChange
          })]
        }), !disableAlpha && /*#__PURE__*/(0, _jsxRuntime.jsx)(_reactColorAlpha["default"], {
          width: 24,
          height: 24,
          hsva: hsva,
          radius: 2,
          style: {
            marginLeft: 4
          },
          bgProps: {
            style: {
              background: 'transparent'
            }
          },
          innerProps: {
            style: styleAlpha
          },
          pointer: function pointer() {
            return /*#__PURE__*/(0, _jsxRuntime.jsx)(_react.Fragment, {});
          }
        })]
      })]
    }), editableDisable && /*#__PURE__*/(0, _jsxRuntime.jsxs)("div", {
      style: {
        display: 'flex',
        margin: '0 10px 3px 10px'
      },
      children: [/*#__PURE__*/(0, _jsxRuntime.jsx)(_reactColorEditableInput["default"], {
        label: "Hex",
        value: (0, _colorConvert.hsvaToHex)(hsva).replace(/^#/, '').toLocaleUpperCase(),
        onChange: function onChange(evn, val) {
          return handleHex(val, evn);
        },
        style: {
          minWidth: 58
        }
      }), /*#__PURE__*/(0, _jsxRuntime.jsx)(_reactColorEditableInputRgba["default"], {
        hsva: hsva,
        style: {
          marginLeft: 6
        },
        aProps: !disableAlpha ? {} : false,
        onChange: function onChange(result) {
          return handleChange(result.hsva);
        }
      })]
    }), presetColors && presetColors.length > 0 && /*#__PURE__*/(0, _jsxRuntime.jsx)(_reactColorSwatch["default"], {
      style: styleSwatch,
      colors: presetColors,
      color: (0, _colorConvert.hsvaToHex)(hsva),
      onChange: function onChange(hsvColor) {
        return handleChange(hsvColor);
      },
      rectProps: {
        style: styleSwatchRect
      }
    })]
  }));
});
Sketch.displayName = 'Sketch';
var _default = exports["default"] = Sketch;
module.exports = exports.default;