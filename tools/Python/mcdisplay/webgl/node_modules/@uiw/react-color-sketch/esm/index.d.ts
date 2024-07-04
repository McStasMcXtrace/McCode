import React from 'react';
import { HsvaColor, ColorResult } from '@uiw/color-convert';
import { SwatchPresetColor } from '@uiw/react-color-swatch';
export interface SketchProps extends Omit<React.HTMLAttributes<HTMLDivElement>, 'onChange' | 'color'> {
    prefixCls?: string;
    width?: number;
    color?: string | HsvaColor;
    presetColors?: false | SwatchPresetColor[];
    editableDisable?: boolean;
    disableAlpha?: boolean;
    onChange?: (newShade: ColorResult) => void;
}
declare const Sketch: React.ForwardRefExoticComponent<SketchProps & React.RefAttributes<HTMLDivElement>>;
export default Sketch;
