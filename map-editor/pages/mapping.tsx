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

interface Columns {
    columns: Column[];
}

interface OnSearch {
    search: string;
    onSearch: (event: any) => void;
}

const Search = ({ search, onSearch }: OnSearch) => {
    return <div>
	<label htmlFor="search">Search columns: </label>
	<input id="search" type="text" onChange={onSearch}
	value ={search} />
    </div>
}

const List = ({columns}: Columns) => {
    return <div> {
	columns.map(column => <Item key={column.column} column={column} />)
    } </div>	
}

const Item = ({ column }: Column) => (
    <div>
	<div>Name: {column.column}</div>
	<div> Source: {column.source}</div>
	<div>Docs: {column.docs}</div>
    </div>
)


    const SearchTerm = ({searchTerm}: string) => {
	if (searchTerm.length == 0) {
	return <div></div>
    } else {
	return <div>
	    Showing results for <strong>{searchTerm}</strong> in column docs
	</div>
    }
}

export default function Home() {

    let [databases, setDatabases] = useState<Database[]>([]);

    const [searchTerm, setSearchTerm] = useState<string>('Something');
    
    const handleSearch = event => {
	setSearchTerm(event.target.value);
    };

    
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

	const table = databases[0].tables[0];

	const columns = table.columns
	
	const filtered_columns = columns.filter((column) => {
	    return column.docs.includes(searchTerm);
	})
	
	return <div>
	    <div>Table name: {table.table}</div>
	    <div> Source:
		<span>Catalog: {table.source.catalog} </span>
		<span>Schema: {table.source.schema}</span>
		<span>Table: {table.source.table}</span>
	    </div>
	    <Search search ={searchTerm} onSearch={handleSearch} />
	    <SearchTerm searchTerm ={searchTerm} />
	    <List columns={filtered_columns} />
	</div>
    }
}

