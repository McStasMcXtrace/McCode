import _extends from "@babel/runtime/helpers/extends";
import _objectWithoutPropertiesLoose from "@babel/runtime/helpers/objectWithoutPropertiesLoose";
var _excluded = ["prefixCls", "className", "onChange", "width", "presetColors", "color", "editableDisable", "disableAlpha", "style"];
import React, { useState, Fragment } from 'react';
import Saturation from '@uiw/react-color-saturation';
import Alpha from '@uiw/react-color-alpha';
import EditableInput from '@uiw/react-color-editable-input';
import RGBA from '@uiw/react-color-editable-input-rgba';
import Hue from '@uiw/react-color-hue';
import { validHex, hsvaToHex, hsvaToRgbaString, hexToHsva, color as handleColor } from '@uiw/color-convert';
import Swatch from '@uiw/react-color-swatch';
import { useEffect } from 'react';
import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
var PRESET_COLORS = ['#D0021B', '#F5A623', '#f8e61b', '#8B572A', '#7ED321', '#417505', '#BD10E0', '#9013FE', '#4A90E2', '#50E3C2', '#B8E986', '#000000', '#4A4A4A', '#9B9B9B', '#FFFFFF'];
var Bar = props => /*#__PURE__*/_jsx("div", {
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
var Sketch = /*#__PURE__*/React.forwardRef((props, ref) => {
  var {
      prefixCls = 'w-color-sketch',
      className,
      onChange,
      width = 218,
      presetColors = PRESET_COLORS,
      color,
      editableDisable = true,
      disableAlpha = false,
      style
    } = props,
    other = _objectWithoutPropertiesLoose(props, _excluded);
  var [hsva, setHsva] = useState({
    h: 209,
    s: 36,
    v: 90,
    a: 1
  });
  useEffect(() => {
    if (typeof color === 'string' && validHex(color)) {
      setHsva(hexToHsva(color));
    }
    if (typeof color === 'object') {
      setHsva(color);
    }
  }, [color]);
  var handleChange = hsv => {
    setHsva(hsv);
    onChange && onChange(handleColor(hsv));
  };
  var handleHex = (value, evn) => {
    if (typeof value === 'string' && validHex(value) && /(3|6)/.test(String(value.length))) {
      handleChange(hexToHsva(value));
    }
  };
  var handleAlphaChange = newAlpha => handleChange(_extends({}, hsva, {
    a: newAlpha.a
  }));
  var handleSaturationChange = newColor => handleChange(_extends({}, hsva, newColor, {
    a: hsva.a
  }));
  var styleMain = _extends({
    '--sketch-background': 'rgb(255, 255, 255)',
    '--sketch-box-shadow': 'rgb(0 0 0 / 15%) 0px 0px 0px 1px, rgb(0 0 0 / 15%) 0px 8px 16px',
    '--sketch-swatch-box-shadow': 'rgb(0 0 0 / 15%) 0px 0px 0px 1px inset',
    '--sketch-alpha-box-shadow': 'rgb(0 0 0 / 15%) 0px 0px 0px 1px inset, rgb(0 0 0 / 25%) 0px 0px 4px inset',
    '--sketch-swatch-border-top': '1px solid rgb(238, 238, 238)',
    background: 'var(--sketch-background)',
    borderRadius: 4,
    boxShadow: 'var(--sketch-box-shadow)',
    width
  }, style);
  var styleAlpha = {
    borderRadius: 2,
    background: hsvaToRgbaString(hsva),
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
  return /*#__PURE__*/_jsxs("div", _extends({}, other, {
    className: prefixCls + " " + (className || ''),
    ref: ref,
    style: styleMain,
    children: [/*#__PURE__*/_jsxs("div", {
      style: {
        padding: '10px 10px 8px'
      },
      children: [/*#__PURE__*/_jsx(Saturation, {
        hsva: hsva,
        style: {
          width: 'auto',
          height: 150
        },
        onChange: handleSaturationChange
      }), /*#__PURE__*/_jsxs("div", {
        style: {
          display: 'flex',
          marginTop: 4
        },
        children: [/*#__PURE__*/_jsxs("div", {
          style: {
            flex: 1
          },
          children: [/*#__PURE__*/_jsx(Hue, {
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
            onChange: newHue => handleChange(_extends({}, hsva, newHue))
          }), !disableAlpha && /*#__PURE__*/_jsx(Alpha, {
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
        }), !disableAlpha && /*#__PURE__*/_jsx(Alpha, {
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
          pointer: () => /*#__PURE__*/_jsx(Fragment, {})
        })]
      })]
    }), editableDisable && /*#__PURE__*/_jsxs("div", {
      style: {
        display: 'flex',
        margin: '0 10px 3px 10px'
      },
      children: [/*#__PURE__*/_jsx(EditableInput, {
        label: "Hex",
        value: hsvaToHex(hsva).replace(/^#/, '').toLocaleUpperCase(),
        onChange: (evn, val) => handleHex(val, evn),
        style: {
          minWidth: 58
        }
      }), /*#__PURE__*/_jsx(RGBA, {
        hsva: hsva,
        style: {
          marginLeft: 6
        },
        aProps: !disableAlpha ? {} : false,
        onChange: result => handleChange(result.hsva)
      })]
    }), presetColors && presetColors.length > 0 && /*#__PURE__*/_jsx(Swatch, {
      style: styleSwatch,
      colors: presetColors,
      color: hsvaToHex(hsva),
      onChange: hsvColor => handleChange(hsvColor),
      rectProps: {
        style: styleSwatchRect
      }
    })]
  }));
});
Sketch.displayName = 'Sketch';
export default Sketch;