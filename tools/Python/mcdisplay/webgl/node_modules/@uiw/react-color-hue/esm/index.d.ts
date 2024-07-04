import React from 'react';
import { AlphaProps } from '@uiw/react-color-alpha';
export interface HueProps extends Omit<AlphaProps, 'hsva' | 'onChange'> {
    onChange?: (newHue: {
        h: number;
    }) => void;
    hue?: number;
}
declare const Hue: React.ForwardRefExoticComponent<HueProps & React.RefAttributes<HTMLDivElement>>;
export default Hue;
