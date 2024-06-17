import {useState, useRef, useEffect} from 'react';
import '../../../common.css';
import './color-picker.css';
import Sketch from '@uiw/react-color-sketch';
import DropDownButton from '../dropdown-button/DropDownButton';

const ColorPicker = () => {
    const [open, setOpen] = useState(false);
    const [color, setColor] = useState('#000000');
    const [tempColor, setTempColor] = useState('#000000');
    const sketchRef = useRef(null);

    const handleOpen = () => {
        setOpen(!open);
    };

    const handleChange = (newShade) => {
        setTempColor(newShade.hex);
    };

    const handleClickOutside = (event) => {
        if (sketchRef.current && !sketchRef.current.contains(event.target)) {
            setColor(tempColor);
        }
    };

    useEffect(() => {
        document.addEventListener('mousedown', handleClickOutside);
        return () => {
            document.removeEventListener('mousedown', handleClickOutside);
        };
    }, [tempColor]);

    return <div id="color-picker" className='row' ref={sketchRef}>
        <DropDownButton id="color-button" text='' color={color} showChevron={false} handleOpen={handleOpen}/>
        {open ? (
        <Sketch                     
            color={tempColor}
            onChange={handleChange}
            />
      ) : null}
    </div>;
};

export default ColorPicker;
