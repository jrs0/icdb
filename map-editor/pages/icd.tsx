import { useState, useRef, useEffect } from 'react';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

const CHECKBOX_STATES = {
    Checked: 'Checked',
    Indeterminate: 'Indeterminate',
    Empty: 'Empty',
};

function Checkbox({ label, value, disabled, onChange }) {

    const checkboxRef = useRef();
    useEffect(() => {
	// Set the contents of the tickbox
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

	// Set whether the element is disabled (grayed out)
	checkboxRef.current.disabled = disabled
	
    }, [value, disabled]);

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

    // Use the exclude status to set the current
    // value of the checkbox. 
    let checked = CHECKBOX_STATES.Checked;
    if (exclude == true) {
	checked = CHECKBOX_STATES.Empty
    }

    // When the checkbox is clicked, update the
    // exclude status, using a callback to modify
    // the top level code_def. The effect will
    // trickle down and rerender the subcomponent
    // checkbox states.
    function handleChange() {
        let updatedChecked;
        if (checked === CHECKBOX_STATES.Checked) {
            // If the item is already checked, and
	    // it is clicked, it should be unchecked.
	    // This is achieved by excluding the current
	    // level
	    exclude_current()
        } else if (checked === CHECKBOX_STATES.Empty) {
	    // If the checkbox is unticked, then either
	    // the current level or 
	    // TODO delete the exclude category for the
            // current level and all child levels
        }
    };

    return <div>
        <div>{cat.code} -- {cat.docs}</div>
	<Checkbox label="Include" onChange={handleChange} value={checked} />
    </div>
}

function Category({ cat, update_code_def, parent_exclude }) {

    // Exclude current level
    function exclude_current() {
	cat.exclude = true;
	setExcluded(true);
	update_code_def()
    }

    // Include the current level and
    // all child levels
    function include_current() {
	delete cat.exclude
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

    // Use the exclude status to set the current
    // value of the checkbox. 
    let checked = CHECKBOX_STATES.Checked;
    if (exclude == true) {
	checked = CHECKBOX_STATES.Empty
    }

    // When the checkbox is clicked, update the
    // exclude status, using a callback to modify
    // the top level code_def. The effect will
    // trickle down and rerender the subcomponent
    // checkbox states.
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
	    include_current()
        }
    };

    // Whether the children of this element are hidden
    let [hidden, setHidden] = useState(true);

    return <div className="category">
        <div>{cat.category} -- {cat.docs}</div>
        <Checkbox label="Include" onChange={handleChange} value={checked} disabled={parent_exclude}/>
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
