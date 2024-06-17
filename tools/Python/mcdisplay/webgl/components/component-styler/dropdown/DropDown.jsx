import {useState} from 'react';
import '../../../common.css';
import './dropdown.css';
import DropDownButton from '../dropdown-button/DropDownButton';

const DropDown = (components) => {
    const [open, setOpen] = useState(false);

    const handleOpen = () => {
      setOpen(!open);
    };

    return <div id="dropdown" className='row'>
        <DropDownButton text="Component 1" handleOpen={handleOpen} />
        {open ? (
        <ul className="menu">
          {components.map((component, index) => (
            <li key={index}>
                <div className='row'>
                    <button onClick={toggleEditComponent}>Component {index}</button>
                </div>
            </li>
          ))}
        </ul>
      ) : null}
    </div>;
};

export default DropDown;
