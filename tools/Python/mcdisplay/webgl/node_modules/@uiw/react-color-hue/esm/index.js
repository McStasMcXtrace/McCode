import _extends from "@babel/runtime/helpers/extends";
import _objectWithoutPropertiesLoose from "@babel/runtime/helpers/objectWithoutPropertiesLoose";
var _excluded = ["prefixCls", "className", "hue", "onChange", "direction"];
import React from 'react';
import Alpha from '@uiw/react-color-alpha';
import { jsx as _jsx } from "react/jsx-runtime";
var Hue = /*#__PURE__*/React.forwardRef((props, ref) => {
  var {
      prefixCls = 'w-color-hue',
      className,
      hue = 0,
      onChange: _onChange,
      direction = 'horizontal'
    } = props,
    other = _objectWithoutPropertiesLoose(props, _excluded);
  return /*#__PURE__*/_jsx(Alpha, _extends({
    ref: ref,
    className: prefixCls + " " + (className || '')
  }, other, {
    direction: direction,
    background: "linear-gradient(to " + (direction === 'horizontal' ? 'right' : 'bottom') + ", rgb(255, 0, 0) 0%, rgb(255, 255, 0) 17%, rgb(0, 255, 0) 33%, rgb(0, 255, 255) 50%, rgb(0, 0, 255) 67%, rgb(255, 0, 255) 83%, rgb(255, 0, 0) 100%)",
    hsva: {
      h: hue,
      s: 100,
      v: 100,
      a: hue / 360
    },
    onChange: (_, interaction) => {
      _onChange && _onChange({
        h: direction === 'horizontal' ? 360 * interaction.left : 360 * interaction.top
      });
    }
  }));
});
Hue.displayName = 'Hue';
export default Hue;