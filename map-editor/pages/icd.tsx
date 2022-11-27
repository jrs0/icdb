import { useState } from 'react';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

function Category({ cat }) {
    return (
        <div class="category">
            <div>{cat.category}</div>
            <div>{cat.docs}</div>
            <ol>
                {
                    cat.child.map((node) => {
                        if ("category" in node) {
                            return (
                                <li>
                                    <Category cat={node} />
                                </li>)
                        } else {
                            return (
                                <li>
                                    <div>{node.code}</div>
                                    <div>{node.docs}</div>
                                </li>)
                        }
                    })
                }
            </ol>
        </div>
    )
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
                    code_def.codes.map((node) => {
                        if ("category" in node) {
                            return (<li><Category cat={node} /></li>)
                        } else {
                            return (<li>Unknown</li>)
                        }
                    })
                }
            </ol>
        </div >
    )

}
