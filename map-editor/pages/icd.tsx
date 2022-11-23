import { useState } from 'react';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'


export default function Home() {

    let [codes, setCodes] = useState([]);

    function load_file() {
        invoke('get_yaml')
            .then(setCodes)
    }

    return (
        <div>
            <Link href="/">Back</Link><br />
            <button onClick={load_file}>Load file</button>
            <h1>ICD-10</h1>
            <ol>

                {codes/* {codes.map((node) => (
                    <li>hello</li>
                    ))} */}
            </ol>
        </div >
    )

}
