import _extends from "@babel/runtime/helpers/extends";
import _objectWithoutPropertiesLoose from "@babel/runtime/helpers/objectWithoutPropertiesLoose";
var _excluded = ["prefixCls", "className", "color", "colors", "style", "rectProps", "onChange", "addonAfter", "addonBefore", "rectRender"];
import React, { Fragment } from 'react';
import { hexToHsva, color as handleColor } from '@uiw/color-convert';
import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
var Swatch = /*#__PURE__*/React.forwardRef((props, ref) => {
  var {
      prefixCls = 'w-color-swatch',
      className,
      color,
      colors = [],
      style,
      rectProps = {},
      onChange,
      addonAfter,
      addonBefore,
      rectRender
    } = props,
    other = _objectWithoutPropertiesLoose(props, _excluded);
  var rectStyle = _extends({
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
  var handleClick = (hex, evn) => {
    onChange && onChange(hexToHsva(hex), handleColor(hexToHsva(hex)), evn);
  };
  return /*#__PURE__*/_jsxs("div", _extends({
    ref: ref
  }, other, {
    className: [prefixCls, className || ''].filter(Boolean).join(' '),
    style: _extends({
      display: 'flex',
      flexWrap: 'wrap',
      position: 'relative'
    }, style),
    children: [addonBefore && /*#__PURE__*/React.isValidElement(addonBefore) && addonBefore, colors && Array.isArray(colors) && colors.map((item, idx) => {
      var title = '';
      var background = '';
      if (typeof item === 'string') {
        title = item;
        background = item;
      }
      if (typeof item === 'object' && item.color) {
        title = item.title || item.color;
        background = item.color;
      }
      var checked = color && color.toLocaleLowerCase() === background.toLocaleLowerCase();
      var render = rectRender && rectRender({
        title,
        color: background,
        checked: !!checked,
        style: _extends({}, rectStyle, {
          background
        }),
        onClick: evn => handleClick(background, evn)
      });
      if (render) {
        return /*#__PURE__*/_jsx(Fragment, {
          children: render
        }, idx);
      }
      var child = rectProps.children && /*#__PURE__*/React.isValidElement(rectProps.children) ? /*#__PURE__*/React.cloneElement(rectProps.children, {
        color: background,
        checked
      }) : null;
      return /*#__PURE__*/_jsx("div", _extends({
        tabIndex: 0,
        title: title,
        onClick: evn => handleClick(background, evn)
      }, rectProps, {
        children: child,
        style: _extends({}, rectStyle, {
          background
        })
      }), idx);
    }), addonAfter && /*#__PURE__*/React.isValidElement(addonAfter) && addonAfter]
  }));
});
Swatch.displayName = 'Swatch';
export default Swatch;