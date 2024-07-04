import React, { FunctionComponent } from "react";
type Style = {
    [key: string]: string;
};
interface PrimaryProps {
    height?: string | number;
    width?: string | number;
    ariaLabel?: string;
    wrapperStyle?: Style;
    wrapperClass?: string;
    visible?: boolean;
}
interface BaseProps extends PrimaryProps {
    color?: string;
}
interface AudioProps extends BaseProps {
}
export const Audio: FunctionComponent<AudioProps>;
interface BallTriangleProps extends BaseProps {
    radius?: string | number;
}
export const BallTriangle: FunctionComponent<BallTriangleProps>;
interface BarsProps extends BaseProps {
}
export const Bars: FunctionComponent<BarsProps>;
interface CirclesProps extends BaseProps {
}
export const Circles: FunctionComponent<CirclesProps>;
type Props = {
    wrapperStyle?: Style;
    visible?: boolean;
    wrapperClass?: string;
    height?: string | number;
    width?: string | number;
    color?: string;
    outerCircleColor?: string;
    innerCircleColor?: string;
    barColor?: string;
    ariaLabel?: string;
};
/**
 * @description contains two circles rotating in opposite direction
 * and a wave bars. outer circle, inner circle and bar
 * color can be set from props.
 */
export const CirclesWithBar: React.FunctionComponent<Props>;
interface GridProps extends BaseProps {
    radius?: string | number;
}
export const Grid: FunctionComponent<GridProps>;
interface HeartsProps extends BaseProps {
}
export const Hearts: FunctionComponent<HeartsProps>;
type _Props1 = {
    color?: string;
    width?: string;
};
export const InfinitySpin: FunctionComponent<_Props1>;
type _Props2 = {
    wrapperStyle?: Style;
    visible?: boolean;
    wrapperClass?: string;
    height?: string | number;
    width?: string | number;
    color?: string;
    firstLineColor?: string;
    middleLineColor?: string;
    lastLineColor?: string;
    ariaLabel?: string;
};
/**
 * @description contains three lines in a wave motion
 * line colors are changeable
 */
export const LineWave: React.FunctionComponent<_Props2>;
interface MutatingDotsProps extends BaseProps {
    radius?: string | number;
    secondaryColor?: string;
}
export const MutatingDots: FunctionComponent<MutatingDotsProps>;
interface OvalProps extends BaseProps {
    strokeWidth?: string | number;
    strokeWidthSecondary?: string | number;
    secondaryColor?: string;
}
export const Oval: FunctionComponent<OvalProps>;
interface PuffProps extends BaseProps {
    radius?: string | number;
    secondaryColor?: string;
}
export const Puff: FunctionComponent<PuffProps>;
interface RevolvingDotProps extends BaseProps {
    radius?: number;
    secondaryColor?: string;
    strokeWidth?: number;
}
export const RevolvingDot: FunctionComponent<RevolvingDotProps>;
interface RingsProps extends BaseProps {
    radius?: string | number;
}
export const Rings: FunctionComponent<RingsProps>;
type RotatingSquareProps = {
    wrapperClass?: string;
    color?: string;
    strokeWidth?: string | number;
    height?: string | number;
    width?: string | number;
    ariaLabel?: string;
    wrapperStyle?: Style;
    visible?: boolean;
};
export const RotatingSquare: React.FunctionComponent<RotatingSquareProps>;
type _Props3 = {
    width?: string;
    visible?: boolean;
    strokeWidth?: string;
    strokeColor?: string;
    animationDuration?: string;
    ariaLabel?: string;
};
export const RotatingLines: FunctionComponent<_Props3>;
interface TailSpinProps extends BaseProps {
    radius?: string | number;
    strokeWidth?: string | number;
}
export const TailSpin: FunctionComponent<TailSpinProps>;
type _Props4 = {
    wrapperStyle?: Style;
    visible?: boolean;
    wrapperClass?: string;
    height?: string | number;
    width?: string | number;
    color?: string;
    outerCircleColor?: string;
    innerCircleColor?: string;
    middleCircleColor?: string;
    ariaLabel?: string;
};
/**
 * @description contains three circles rotating in opposite direction
 * outer circle, middle circle and inner circle color can be set from props.
 */
export const ThreeCircles: React.FunctionComponent<_Props4>;
interface ThreeDotsProps extends BaseProps {
    radius?: string | number;
}
export const ThreeDots: FunctionComponent<ThreeDotsProps>;
type TriangleProps = BaseProps;
/** Styles Ends */
export const Triangle: FunctionComponent<TriangleProps>;
interface WatchProps extends BaseProps {
    radius?: string | number;
}
export const Watch: FunctionComponent<WatchProps>;
type FallingLinesProps = {
    color?: string;
    width?: string;
    height?: string;
    visible?: boolean;
};
export const FallingLines: FunctionComponent<FallingLinesProps>;
interface VortexProps extends Omit<BaseProps, 'color'> {
    colors?: [string, string, string, string, string, string];
}
export const Vortex: FunctionComponent<VortexProps>;
interface RotatingTrianglesProps extends Omit<BaseProps, 'color'> {
    colors?: [string, string, string];
}
export const RotatingTriangles: FunctionComponent<RotatingTrianglesProps>;
interface RadioProps extends Omit<BaseProps, 'color'> {
    colors?: [string, string, string];
}
export const Radio: FunctionComponent<RadioProps>;
interface ProgressBarProps extends Omit<BaseProps, 'color'> {
    borderColor?: string;
    barColor?: string;
}
export const ProgressBar: FunctionComponent<ProgressBarProps>;
interface MagnifyingGlassProps extends BaseProps {
    glassColor?: string;
}
export const MagnifyingGlass: FunctionComponent<MagnifyingGlassProps>;
interface FidgetSpinnerProps extends Omit<BaseProps, 'color'> {
    backgroundColor?: string;
    ballColors?: [string, string, string];
}
export const FidgetSpinner: FunctionComponent<FidgetSpinnerProps>;
interface DNAProps extends Omit<BaseProps, 'color'> {
}
export const DNA: FunctionComponent<DNAProps>;
interface DiscussProps extends Omit<BaseProps, 'color'> {
    colors?: [string, string];
}
export const Discuss: FunctionComponent<DiscussProps>;
interface ColorRingProps extends Omit<BaseProps, 'color'> {
    colors?: [string, string, string, string, string];
}
export const ColorRing: FunctionComponent<ColorRingProps>;
interface CommentProps extends BaseProps {
    backgroundColor?: string;
}
export const Comment: FunctionComponent<CommentProps>;
interface BlocksProps extends BaseProps {
}
export const Blocks: FunctionComponent<BlocksProps>;
interface HourglassProps extends Omit<BaseProps, 'color'> {
    colors?: [string, string];
}
export const Hourglass: FunctionComponent<HourglassProps>;

//# sourceMappingURL=types.d.ts.map
