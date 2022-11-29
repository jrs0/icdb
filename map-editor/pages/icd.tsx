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

function Category({ cat, n, parent_checked, parent_write_exclude }) {
    // checked is the main controlling state, which
    // stores the exclusion status of the current level
    let [checked, setChecked] = useState(CHECKBOX_STATES.Checked);

    // The write_exclude function is used to write an exclude
    // key into the codes definition file. write_exclude
    // takes
    function write_exclude(indices) {
        indices.push(n)
        parent_write_exclude(indices)
    }

    // It is really important that you check whether
    // checked is empty first, otherwise you get in an
    // infinite loop of rerenders for this element (think
    // about it). This is a structural problem -- to fix
    // later.
    /* if (checked !== CHECKBOX_STATES.Empty &&
     *     parent_checked === CHECKBOX_STATES.Empty) {
     *     setChecked(CHECKBOX_STATES.Empty)
     * }
     */

    function handleChange() {
        let updatedChecked;
        if (checked === CHECKBOX_STATES.Checked) {
            updatedChecked = CHECKBOX_STATES.Empty;
            // TODO write an exclude key for the current
            // category
            //setCurrentExclude(true);
            console.log("n=", n)
            parent_write_exclude([n])
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
            cat.child.map((node, index) => {
                if (!hidden) {
                    if ("category" in node) {
                        return <li><Category cat={node}
                            parent_checked={checked}
                            parent_write_exclude={write_exclude}
                            n={index}
                        /></li>
                    } else {
                        return <li><Code code={node}
                            parent_checked={checked} /></li>
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

    function write_exclude(indices) {
        console.log(indices)
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
                <li><Category cat={code_def.codes[0]} n={0}
                    parent_write_exclude={write_exclude} /></li>
            </ol>
        </div>
    }
}
