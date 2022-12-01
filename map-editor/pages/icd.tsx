import { useState, useRef, useEffect } from 'react';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

const CHECKBOX_STATES = {
    Checked: 'Checked',
    Indeterminate: 'Indeterminate',
    Empty: 'Empty',
};

function Checkbox({ label, checked, enabled, onChange }) {

    const checkboxRef = useRef();
    useEffect(() => {

	// Set whether the element is disabled (grayed out)
	checkboxRef.current.disabled = !enabled
	
    }, [enabled]);

    return (
        <label>
            <input
                ref={checkboxRef}
                type="checkbox"
                checked={checked}
                onChange={onChange}
            />
            {label}
        </label>
    );
};

// Establish whether the component should be included
// (i.e. ticked) and whether it should be enabled
// (grayed out or not)
function visible_status(cat, parent_exclude, refresh_code_def)
{
    // Component is included by default, unless there
    // is an exclude tag at the current level, or
    // the parent is excluded
    let exclude_tag = ("exclude" in cat) && (cat.exclude == true);
    let included = !exclude_tag && !parent_exclude
    
    // Checkbox is enabled if the parent is not excluded
    let enabled = !parent_exclude;

    return {
	included: included,
	enabled: enabled
    }
}

function Code({ cat, parent_exclude }) {


    function handleChange() {

    };

    let {included, enabled} = visible_status(cat, parent_exclude);
    
    return <div>
        <div>{cat.code} -- {cat.docs}</div>
	<Checkbox label="Include" onChange={handleChange} checked={included} enabled={enabled} />
    </div>
}

function Category({ cat, parent_exclude, refresh_code_def }) {
        
    // Whether the children of this element are hidden
    let [hidden, setHidden] = useState(true);

    let {included, enabled} = visible_status(cat, parent_exclude);

    // Take action when the user clicks the checkbox. Note that
    // this function cannot be called for a grayed out box,
    // because it cannot change. This means you can assume the
    // current level is enabled, meaning that none of the parents
    // are excluded.
    function handleChange() {

	// Check the current state of the checkbox
	if (included) {
	    // When the current component is included,
	    // the user is wanting to disable this element,
	    // and all of its subcomponents. This involves
	    // writing an exclude tag into the current
	    // level, and clearning any exclude flags
	    // in subcomponent levels (for efficiency
	    // of representation)
	    console.log("I am included")

	    // Set the current exclude tag
	    cat.exclude = true;

	    // Refresh the state of the code_def
	    // structure so that all components
	    // rerender
	    refresh_code_def()
	    
	} else {

	    console.log("I am excluded")

	    // Remove any exclude tag if it exists
	    delete cat.exclude;

	    // Refresh the state of the code_def
	    // structure so that all components
	    // rerender
	    refresh_code_def()
	    
	}	
    }
    
    return <div className="category">
        <div>{cat.category} -- {cat.docs}</div>
        <Checkbox label="Include" onChange={handleChange} checked={included} enabled={enabled}/>
        <button onClick={() => setHidden(!hidden)}>Toggle Hidden</button>
        <ol> {
            cat.child.map((node) => {
                if (!hidden) {
                    if ("category" in node) {
                        return <li><Category cat={node} parent_exclude={!included} refresh_code_def={refresh_code_def}/></li>
                    } else {
                        return <li><Code cat={node} parent_exclude={!included} refresh_code_def={refresh_code_def}/></li>
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

    // To be called from subcomponents after they have
    // modified code_def by references (by changing cat
    // objects)
    function refresh_code_def() {
	console.log(code_def)
	setCodeDef(code_def)
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
			      parent_exclude={false}
	refresh_code_def={refresh_code_def} /></li>
            </ol>
        </div>
    }
}
