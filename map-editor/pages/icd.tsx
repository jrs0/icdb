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
                checked={value === CHECKBOX_STATES.Checked}
                onChange={onChange}
            />
            {label}
        </label>
    );
};

function Code({ code, exclude }) {

    // Check whether the code is excluded
    if ("exclude" in code) {
        if (code.exclude == true) {
            exclude = true;
        }
    }

    return <div>
        <div>{code.code} -- {code.docs}</div>
        <input type="checkbox" checked={!exclude} />
    </div>
}

// cat -- the category to render
// parent_exclude -- means the parent is excluded
function Category({ cat, parent_checked = CHECKBOX_STATES.Checked }) {

    // The parent_checked is the state of the 

    // This state states whether the current level is
    // included or excluded. It is passed into descendants
    // to tell them 
    /* let [current_exclude,
     *     setCurrentExclude] = useState(parent_exclude); */

    // Include the current level and all descendands
    // 
    function include_current_and_descendants() {

    }

    // Whether to exclude this level or not
    // value either derives from exclude flag at this
    // level, or whether the exclusion is inherited
    //let exclude = current_exclude | parent_exclude;

    // Check whether the category is
    // excluded. If it is, pass this
    // information down to the child categories
    //let exclude = (parent_checked === CHECKBOX_STATES.Empty);
    //let checked_initial = parent_checked;
    /* if ("exclude" in cat) {
       if (cat.exclude == true) {
     *         exclude = true;
       }
     * } */

    // Calculate initial value of checkbox
    /* if (exclude) {
     *     checked_initial = CHECKBOX_STATES.Empty
     * } */

    // checked is the main controlling state, which
    // stores the exclusion status of the current level
    let [checked, setChecked] = useState(CHECKBOX_STATES.Checked);

    // Handle the transitions of the include state. The tick
    // is displayed if none of the subcategories are excluded.
    // The valid transition are (dash means indeterminate)
    //
    // tick -> untick
    // dash -> untick
    // untick -> tick
    //
    // On a transition to tick, all the subcategories are
    // included, so any exclude flags in the subcategories
    // are cleared. On a transition to untick, an exclude is
    // written into the current level. It is not possible to
    // transition to indeterminate. This occurs when one of
    // the excluded, but others are included.
    //
    function handleChange() {
        let updatedChecked;
        if (checked === CHECKBOX_STATES.Checked) {
            updatedChecked = CHECKBOX_STATES.Empty;
            // TODO write an exclude key for the current
            // category
            //setCurrentExclude(true);
        } else if (checked === CHECKBOX_STATES.Empty) {
            updatedChecked = CHECKBOX_STATES.Checked;
            // TODO delete the exclude category for the
            // current level and all child levels
        }
        setChecked(updatedChecked);
    };

    let [hidden, setHidden] = useState(true);

    return <div class="category">
        <div>{cat.category} -- {cat.docs}</div>
        <Checkbox label="Include" onChange={handleChange} value={checked} />
        <button onClick={() => setHidden(!hidden)}>Toggle Hidden</button>
        <ol> {
            cat.child.map((node) => {
                if (!hidden) {
                    if ("category" in node) {
                        return <li><Category cat={node}
                            parent_checked={checked} /></li>
                    } else {
                        return <li><Code code={node} exclude={exclude} /></li>
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
                <li><Category cat={code_def.codes[0]} /></li>
            </ol>
        </div>
    }
}
