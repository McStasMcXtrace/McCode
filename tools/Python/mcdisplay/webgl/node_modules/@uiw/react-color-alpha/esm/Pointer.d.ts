import React from 'react';
export interface PointerProps extends React.HTMLAttributes<HTMLDivElement> {
    prefixCls?: string;
    left?: string;
    top?: string;
    fillProps?: React.HTMLAttributes<HTMLDivElement>;
}
export declare const Pointer: ({ className, prefixCls, left, top, style, fillProps, ...reset }: PointerProps) => JSX.Element;
