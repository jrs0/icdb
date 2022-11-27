import { useState } from 'react';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

function Code({ code }) {
    return <div>
        <div>{code.code} -- {code.docs}</div>
        <input type="checkbox" value=checked" />
    </div>
}

function Category({ cat }) {

    return <div class="category">
        <div>{cat.category} -- {cat.docs}</div>
        <input type="checkbox" value="checked" />
        <ol> {
            cat.child.map((node) => {
                if ("category" in node) {
                    return <li><Category cat={node} /></li>
                } else {
                    return <li><Code code={node} /></li>
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
        return code_defs.groups
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
            <Category cat={code_def.codes[0]} />
        </div>
    }
}
