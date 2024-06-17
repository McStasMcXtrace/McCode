import React from 'react';
import '../../common.css';
import './component-styler.css';
import { useComponentsContext } from '../../Contexts/ComponentsContext';
import DropDown from './dropdown/DropDown';
import ColorPicker from './color-picker/ColorPicker';

const ComponentStyler = () => {
    const {components, setComponents} = useComponentsContext();

    return <div id="component-styler" className='row'>
        <DropDown components={components}/>
        <ColorPicker/>
    </div>;
};

export default ComponentStyler;