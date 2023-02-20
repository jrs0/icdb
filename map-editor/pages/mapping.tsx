import Link from 'next/link'

interface Database {
    docs: string[];
    exclude?: string[];
    child?: Cat[];
    category?: string;
    code?: string;
    docs: string;
    index: string;
}

export default function Home() {

    let [mapping, setMapping] = useState<Cat>({docs: "None",
					       index: "None"});
    
    function save_file() {
        invoke('save_yaml', { codeDef: code_def })
    }

    function load_file() {
        invoke('get_yaml')
	    .then((result) => {

		let res: Cat = JSON.parse(result as string);
		console.log(res)
		// Note: all .then are executed
		// asynchronously, so put
		// sequential steps in here
		if (res.groups !== undefined) {
		    if (res.groups.length > 0) {
			setGroup(res.groups[0])
		    } else {
			alert("No groups found. Add some groups and reload the file.")
			return
		    }
		} else {
		    alert("Did not find groups key. Add a groups key containing an array of groups.")
		    return
		}
		// If you get here, then the state is valid
		setCodeDef(res)

	    })
    }
    
    
    return <p>Todo</p>
}

