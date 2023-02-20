import { useState, useRef, useEffect, useMemo, ChangeEvent } from 'react';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

import styles from '../styles/Category.module.css'

interface Source {
    catalog: string;
    schema: string;
    table: string;
}

interface Column {
    column: string;
    docs: string;
    use: string;
    source: string[]
    strategy: string
}

interface Table {
    table: string;
    source: Source;
    columns: Column[]
}

interface Database {
    database: string;
    docs: string;
    tables: Table[];
}

interface Mapping {
    databases: Database[]
}

export default function Home() {

    let [mapping, setMapping] = useState<Mapping>([]);
    
    function save_file() {
        invoke('save_yaml', { codeDef: code_def })
    }

    function load_file() {
        invoke('get_yaml')
	    .then((result) => {

		let res: Cat = JSON.parse(result as string);
		console.log(res)
		setMapping(res)

	    })
    }
    
    
    return <div>
        <h1>Database Mapping Editor</h1>
	<p className={styles.info}>Load a mapping file to set up databases tables and columns.</p>
	<div>
	    <span className={styles.button}
		  onClick={load_file}>Load file</span>
	    <Link className={styles.button} href="/">Back</Link>
	</div>
    </div>

}

