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

export default function Home() {

    let [databases, setDatabases] = useState<Database[]>([]);
    
    function save_file() {
        invoke('save_yaml', { codeDef: code_def })
    }

    function load_file() {
        invoke('get_yaml')
	    .then((result) => {

		let res: Cat = JSON.parse(result as string);
		console.log(res)
		setDatabases(res)

	    })
    }
    

    if (databases.length == 0) {
	return <div>
            <h1>Database Mapping Editor</h1>
	    <p className={styles.info}>Load a mapping file to set up databases tables and columns.</p>
	    <div>
		<span className={styles.button}
		      onClick={load_file}>Load file</span>
		<Link className={styles.button} href="/">Back</Link>
	    </div>
	</div>
    } else {

	let table = databases[0].tables[0];
	
	return <div>
	    <div>Table name: {table.table}</div>
	    <div> Source:
		<span>Catalog: {table.source.catalog} </span>
		<span>Schema: {table.source.schema}</span>
		<span>Table: {table.source.table}</span>
	    </div>
	    <label htmlFor="search">Search columns: </label>
	    <input id="search" type="text" />
	    <div> {
		table.columns.map((column) => {
		    return <div key ={column.column}>
			<div>Name: {column.column}</div>
			<div> Source: {column.source}</div>
			<div>Docs: {column.docs}</div>
			
		    </div>;
		})
	    } </div>	
	</div>
    }
}

