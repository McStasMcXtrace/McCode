import React from 'react';
import '../../../common.css';
import './dropdown-button.css';
import Chevron from './chevron.svg'; 

const DropDownButton = ({id, text, showChevron=true, handleOpen, color}) => {

    function getGradientColor(color) {
        const transparentColor = color === 'black' ? 'rgba(0, 0, 0, 0)' : 'rgba(255, 255, 255, 0)';
        return `linear-gradient(-45deg, ${color}, ${transparentColor})`;
    }
    
    const style = {
        background: getGradientColor(color),
    };

    return <>
        <button             
            id={id}
            className="dropdown-button"
            onClick={handleOpen}
            style={style}
        >
            <div className='row'>
                {text} 
                {showChevron && <img src={Chevron} alt="Chevron" className="chevron-icon" />}
            </div>
        </button>
    </>;
};

export default DropDownButton;
