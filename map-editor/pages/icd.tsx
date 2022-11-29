import { useState, useRef } from 'react';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

const CHECKBOX_STATES = {
    Checked: 'Checked',
    Indeterminate: 'Indeterminate',
    Empty: 'Empty',
};

function Checkbox({ label, value, onChange }) {
    return (
        <label>
            <input
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

function Category({ cat, exclude }) {

    let [checked, setChecked] = useState(CHECKBOX_STATES.Empty);

    function handleChange() {
        let updatedChecked;

        if (checked === CHECKBOX_STATES.Checked) {
            updatedChecked = CHECKBOX_STATES.Empty;
        } else if (checked === CHECKBOX_STATES.Empty) {
            updatedChecked = CHECKBOX_STATES.Checked;
        }

        setChecked(updatedChecked);
    };

    let [hidden, setHidden] = useState(true);

    // Check whether the category is
    // excluded. If it is, pass this
    // information down to the child categories
    if ("exclude" in cat) {
        if (cat.exclude == true) {
            exclude = true;
        }
    }

    // Select or unselect the current level
    // in the particular group
    function toggle_selected() {
        setSelected(!selected)
    }

    return <div class="category">
        <div>{cat.category} -- {cat.docs}</div>
        <Checkbox label="Include" onChange={handleChange} value={checked} />
        <button onClick={() => setHidden(!hidden)}>Toggle Hidden</button>
        <ol> {
            cat.child.map((node) => {
                if (!hidden) {
                    if ("category" in node) {
                        return <li><Category cat={node}
                            exclude={exclude} /></li>
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
