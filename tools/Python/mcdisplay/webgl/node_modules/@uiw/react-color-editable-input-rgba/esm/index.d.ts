import React from 'react';
import { EditableInputProps } from '@uiw/react-color-editable-input';
import { HsvaColor, ColorResult } from '@uiw/color-convert';
export interface EditableInputRGBAProps extends Omit<React.HTMLAttributes<HTMLDivElement>, 'onChange'> {
    prefixCls?: string;
    hsva: HsvaColor;
    placement?: 'top' | 'left' | 'bottom' | 'right';
    rProps?: EditableInputProps;
    gProps?: EditableInputProps;
    bProps?: EditableInputProps;
    aProps?: false | EditableInputProps;
    onChange?: (color: ColorResult) => void;
}
declare const EditableInputRGBA: React.ForwardRefExoticComponent<EditableInputRGBAProps & React.RefAttributes<HTMLDivElement>>;
export default EditableInputRGBA;
