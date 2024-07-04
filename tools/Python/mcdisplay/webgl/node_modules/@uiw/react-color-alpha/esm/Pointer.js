import _extends from "@babel/runtime/helpers/extends";
import _objectWithoutPropertiesLoose from "@babel/runtime/helpers/objectWithoutPropertiesLoose";
var _excluded = ["className", "prefixCls", "left", "top", "style", "fillProps"];
import React from 'react';
import { jsx as _jsx } from "react/jsx-runtime";
export var Pointer = _ref => {
  var {
      className,
      prefixCls,
      left,
      top,
      style,
      fillProps
    } = _ref,
    reset = _objectWithoutPropertiesLoose(_ref, _excluded);
  var styleWrapper = _extends({}, style, {
    position: 'absolute',
    left,
    top
  });
  var stylePointer = _extends({
    width: 18,
    height: 18,
    boxShadow: 'var(--alpha-pointer-box-shadow)',
    borderRadius: '50%',
    backgroundColor: 'var(--alpha-pointer-background-color)'
  }, fillProps == null ? void 0 : fillProps.style, {
    transform: left ? 'translate(-9px, -1px)' : 'translate(-1px, -9px)'
  });
  return /*#__PURE__*/_jsx("div", _extends({
    className: prefixCls + "-pointer " + (className || ''),
    style: styleWrapper
  }, reset, {
    children: /*#__PURE__*/_jsx("div", _extends({
      className: prefixCls + "-fill"
    }, fillProps, {
      style: stylePointer
    }))
  }));
};