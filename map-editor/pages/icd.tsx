import { useState, useRef, useEffect } from 'react';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

const CHECKBOX_STATES = {
    Checked: 'Checked',
    Indeterminate: 'Indeterminate',
    Empty: 'Empty',
};

function Checkbox({ label, value, onChange }) {

    const checkboxRef = useRef();
    useEffect(() => {
        if (value === CHECKBOX_STATES.Checked) {
            checkboxRef.current.checked = true;
            checkboxRef.current.indeterminate = false;
        } else if (value === CHECKBOX_STATES.Empty) {
            checkboxRef.current.checked = false;
            checkboxRef.current.indeterminate = false;
        } else if (value === CHECKBOX_STATES.Indeterminate) {
            checkboxRef.current.checked = false;
            checkboxRef.current.indeterminate = true;
        }
    }, [value]);

    return (
        <label>
            <input
                ref={checkboxRef}
                type="checkbox"
                value={value}
                onChange={onChange}
            />
            {label}
        </label>
    );
};

function Code({ cat, update_code_def, parent_exclude }) {

    // Set the state of the checkbox is controlled by the
    // exclude variable. If the current
    // level or any of the parent levels are excluded,
    // then set the checkbox to unticked
    let exclude = false
    if (parent_exclude == true || cat.exclude == true) {
	exclude = true
    }
    
    let checked = CHECKBOX_STATES.Checked;
    if (exclude == true) {
	checked = CHECKBOX_STATES.Empty
    }
    
    function handleChange() {
        let updatedChecked;
        if (checked === CHECKBOX_STATES.Checked) {
            updatedChecked = CHECKBOX_STATES.Empty;
            // TODO write an exclude key for the current
            // category
	    exclude_current()
        } else if (checked === CHECKBOX_STATES.Empty) {
            updatedChecked = CHECKBOX_STATES.Checked;
            // TODO delete the exclude category for the
            // current level and all child levels
        }
        //setChecked(updatedChecked);
    };

    function handleChange() {
        let updatedChecked;
        if (checked === CHECKBOX_STATES.Checked) {
            updatedChecked = CHECKBOX_STATES.Empty;
            // TODO write an exclude key for the current
            // category
        } else if (checked === CHECKBOX_STATES.Empty) {
            updatedChecked = CHECKBOX_STATES.Checked;
            // TODO delete the exclude category for the
            // current level and all child levels
        }
        setChecked(updatedChecked);
    };  

    return <div>
        <div>{cat.code} -- {cat.docs}</div>
	<Checkbox label="Include" onChange={handleChange} value={checked} />
    </div>
}

function Category({ cat, update_code_def, parent_exclude }) {
    // checked is the main controlling state, which
    // stores the exclusion status of the current level

    // Exclude current level
    function exclude_current() {
	cat.exclude = true;
	setExcluded(true);
	update_code_def()
    }

    // Set the state of the checkbox is controlled by the
    // exclude variable. If the current
    // level or any of the parent levels are excluded,
    // then set the checkbox to unticked
    let [exclude, setExcluded] = useState(false);
    if (exclude == false && (parent_exclude == true || cat.exclude == true)) {
	setExcluded(true)
    }
    
    let checked = CHECKBOX_STATES.Checked;
    if (exclude == true) {
	checked = CHECKBOX_STATES.Empty
    }
    
    function handleChange() {
        let updatedChecked;
        if (checked === CHECKBOX_STATES.Checked) {
            updatedChecked = CHECKBOX_STATES.Empty;
            // TODO write an exclude key for the current
            // category
	    exclude_current()
        } else if (checked === CHECKBOX_STATES.Empty) {
            updatedChecked = CHECKBOX_STATES.Checked;
            // TODO delete the exclude category for the
            // current level and all child levels
        }
        //setChecked(updatedChecked);
    };

    // Whether the children of this element are hidden
    let [hidden, setHidden] = useState(true);

    return <div className="category">
        <div>{cat.category} -- {cat.docs}</div>
        <Checkbox label="Include" onChange={handleChange} value={checked} />
        <button onClick={() => setHidden(!hidden)}>Toggle Hidden</button>
        <ol> {
            cat.child.map((node) => {
                if (!hidden) {
                    if ("category" in node) {
                        return <li><Category cat={node} update_code_def={update_code_def} parent_exclude={exclude} /></li>
                    } else {
                        return <li><Code cat={node} update_code_def={update_code_def} parent_exclude={exclude}/></li>
                    }
                }
            })
        } </ol>
    </div>
}

export default function Home() {

    let [code_def, setCodeDef] = useState(0);

    // Function to load the codes yaml file
    function load_file() {
        invoke('get_yaml')
            .then(JSON.parse)
            .then(setCodeDef)
    }

    // Function to get the list of groups
    function get_groups() {
        return code_def.groups
    }

    function update_code_def() {
	setCodeDef(code_def)
	console.log(code_def)
    }

    if (code_def == 0) {
        return <div>
            <Link href="/">Back</Link><br />
            <h1>ICD-10</h1>
            <button onClick={load_file}>Load file</button>
        </div>
    } else {
        return <div>
            <Link href="/">Back</Link><br />
            <h1>ICD-10</h1>
            <div>Groups: {get_groups()}</div>
            <ol>
                <li><Category cat={code_def.child[0]}
			      update_code_def={update_code_def}
		    parent_exclude={false} /></li>
            </ol>
        </div>
    }
}
