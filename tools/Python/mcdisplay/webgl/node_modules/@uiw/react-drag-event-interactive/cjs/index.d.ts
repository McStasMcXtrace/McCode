import React from 'react';
import { Interaction } from './utils';
export * from './utils';
export interface InteractiveProps extends React.HTMLAttributes<HTMLDivElement> {
    prefixCls?: string;
    onMove?: (interaction: Interaction, event: MouseEvent | TouchEvent) => void;
    onDown?: (offset: Interaction, event: MouseEvent | TouchEvent) => void;
}
declare const Interactive: React.ForwardRefExoticComponent<InteractiveProps & React.RefAttributes<HTMLDivElement>>;
export default Interactive;
