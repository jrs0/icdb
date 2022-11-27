import { useState } from 'react';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

function Category({ name }) {
    return <div>{name}</div>
}

export default function Home() {

    let [code_def, setCodeDef] = useState({ "groups": [], "codes": [] });

    function load_file() {
        invoke('get_yaml')
            .then(JSON.parse)
            .then(setCodeDef)
    }

    return (
        <div>
            <Link href="/">Back</Link><br />
            <button onClick={load_file}>Load file</button>
            <h1>ICD-10</h1>
            <ol>
                {
                    code_def.codes.map((node) => (
                        <li><Category name="Hello" /></li>
                    ))
                }
            </ol>
        </div >
    )

}
