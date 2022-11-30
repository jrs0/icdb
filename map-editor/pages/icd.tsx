import { useState, useRef, useEffect } from 'react';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

const CHECKBOX_STATES = {
    Checked: 'Checked',
    Indeterminate: 'Indeterminate',
    Empty: 'Empty',
};

function Checkbox({ label, value, enabled, onChange }) {

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
	checkboxRef.current.disabled = !enabled
	
    }, [value, enabled]);

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

function Code({ cat, parent_exclude }) {

    // The exclude state determines whether the
    // current level is excluded or not. The
    // exclusion may come from the presence of
    // an exclude tag, or it may come from the
    // parent being excluded.
    let [exclude, setExcluded] = useState(false);

    // Exclude current level
    function exclude_current() {

	// Add the exclude tag for the
	// current level
	cat.exclude = true;
	
	// Update the state of the current
	// level to be excluded. This will
	// trigger render of the child
	// components
	setExcluded(true);
    }

    function include_current() {
	delete cat.exclude;
	setExcluded(false)
    }


    // All components must store their included
    // state, in order to know how to render the
    // checkbox and handle clicks. However, not
    // every level in the codes definition file
    // (the cat variable) stores an exclude tag.
    // If an exclude tag is present, then it excludes
    // the current level and all child levels.
    // Use the exclude tag to set the excluded
    // state of the current component, before using
    // the parent_exclude
    if (exclude == false) {
	if (cat.exclude || parent_exclude) {
	    exclude_current()
	}
    } else {
	if (!cat.exclude && !parent_exclude) {
	    include_current()   
	}			    
    }			    
    
    // The state of the
    // checkbox is controlled by the exclude state
    // and the parent exclude state. If the exclude
    // is false, then the checkbox is ticked and
    // enabled. If the exclude is true, but the
    // parent exclude is false, then the checkbox
    // is unticked and enabled. If the parent is
    // also excluded, then the checkbox is also
    // disabled.
    let checked = CHECKBOX_STATES.Checked;
    let enabled = true;
    if (exclude == false) {
	checked = CHECKBOX_STATES.Checked;
    } else {
	// This reflects that fact that the user
	// must enable the next level up before
	// modifying this level
	checked = CHECKBOX_STATES.Empty
	enabled = !parent_exclude;
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

    return <div>
        <div>{cat.code} -- {cat.docs}</div>
	<Checkbox label="Include" onChange={handleChange} value={checked} enabled={enabled} />
    </div>
}

function Category({ cat, parent_exclude }) {
        
    // The exclude state determines whether the
    // current level is excluded or not. The
    // exclusion may come from the presence of
    // an exclude tag, or it may come from the
    // parent being excluded.
    let [exclude, setExcluded] = useState(false);

    // Exclude current level
    function exclude_current() {

	// Add the exclude tag for the
	// current level
	cat.exclude = true;
	
	// Update the state of the current
	// level to be excluded. This will
	// trigger render of the child
	// components
	if (exclude != true) {
	    setExcluded(true);
	}
    }

    function include_current() {
	delete cat.exclude;
	if (exclude != false) {
	    setExcluded(false)
	}
    }


    // All components must store their included
    // state, in order to know how to render the
    // checkbox and handle clicks. However, not
    // every level in the codes definition file
    // (the cat variable) stores an exclude tag.
    // If an exclude tag is present, then it excludes
    // the current level and all child levels.
    // Use the exclude tag to set the excluded
    // state of the current component, before using
    // the parent_exclude
    if ("exclude" in cat && cat.exclude == true) {
	exclude_current()
    } else if (parent_exclude == true) {
	// Set as excluded, but do not
	// write to the file
	if (exclude != true) {
	    setExcluded("true")
	}
    } else {
	include_current()
    }
    
    // The state of the
    // checkbox is controlled by the exclude state
    // and the parent exclude state. If the exclude
    // is false, then the checkbox is ticked and
    // enabled. If the exclude is true, but the
    // parent exclude is false, then the checkbox
    // is unticked and enabled. If the parent is
    // also excluded, then the checkbox is also
    // disabled.
    let checked = CHECKBOX_STATES.Checked;
    let enabled = true;
    if (exclude == false) {
	checked = CHECKBOX_STATES.Checked;
    } else {
	// This reflects that fact that the user
	// must enable the next level up before
	// modifying this level
	checked = CHECKBOX_STATES.Empty
	enabled = !parent_exclude;
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
        <Checkbox label="Include" onChange={handleChange} value={checked} enabled={enabled}/>
        <button onClick={() => setHidden(!hidden)}>Toggle Hidden</button>
        <ol> {
            cat.child.map((node) => {
                if (!hidden) {
                    if ("category" in node) {
                        return <li><Category cat={node}  parent_exclude={exclude} /></li>
                    } else {
                        return <li><Code cat={node}  parent_exclude={exclude}/></li>
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

    // Function to save the codes yaml file
    function save_file() {
        invoke('save_yaml', {codeDef: code_def})
	    .then(console.log("done"))
    }
    
    // Function to get the list of groups
    function get_groups() {
        return code_def.groups
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
	    <button onClick={save_file}>Save as</button>

            <h1>ICD-10</h1>
            <div>Groups: {get_groups()}</div>
            <ol>
                <li><Category cat={code_def.child[0]}
		    parent_exclude={false} /></li>
            </ol>
        </div>
    }
}
