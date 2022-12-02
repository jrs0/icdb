import { useState, useRef, useEffect, useMemo } from 'react';
//import { structuredClone } from '@ungap/structured-clone';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

const CHECKBOX_STATES = {
    Checked: 'Checked',
    Indeterminate: 'Indeterminate',
    Empty: 'Empty',
};

function Checkbox({ label, checked, enabled, onChange }) {

    const checkboxRef = useRef();
    useEffect(() => {

	// Set whether the element is disabled (grayed out)
	checkboxRef.current.disabled = !enabled
	
    }, [enabled]);

    return (
        <label>
            <input
                ref={checkboxRef}
                type="checkbox"
                checked={checked}
                onChange={onChange}
            />
            {label}
        </label>
    );
};

// Establish whether the component should be included
// (i.e. ticked) and whether it should be enabled
// (grayed out or not)
function visible_status(cat, parent_exclude)
{
    // Component is included by default, unless there
    // is an exclude tag at the current level, or
    // the parent is excluded
    let exclude_tag = ("exclude" in cat) && (cat.exclude == true);
    let included = !exclude_tag && !parent_exclude
    
    // Checkbox is enabled if the parent is not excluded
    let enabled = !parent_exclude;

    return {
	included: included,
	enabled: enabled
    }
}

// Remove all the exclude tags in all
// sublevels of cat and return the result
function remove_all_excludes(cat) {

    // Remove the exclude key from this
    // level
    delete cat.exclude;
    
    if ("child" in cat) {
	// Loop over all the subcategories
	// remove the exclude
	cat.child = cat.child.map(remove_all_excludes)
    }

    // Return the modified category
    return(cat)
}

// Set the top-level excludes for the
// subcategories in the current category,
// and return the modified object
function set_first_excludes(cat) {
    if ("child" in cat) {
	cat.child = cat.child.map((subcat) => {
	    subcat.exclude = true
	    return(subcat)
	})
    }
    return(cat)
}

function Code({ cat, parent_exclude }) {


    function handleChange() {

    };

    let {included, enabled} = visible_status(cat, parent_exclude);
    
    return <div>
        <div>{cat.code} -- {cat.docs}</div>
	<Checkbox label="Include" onChange={handleChange} checked={included} enabled={enabled} />
    </div>
}

function Category({ index, cat, parent_exclude, toggle_cat }) {
    
    // BUG: cat is being passed as cat_init to the
    // next level down, but then that is only being
    // used to initialise the state one level down.
    // What we want is to inherit the structure of
    // cat from the level above. Why does cat even
    // need to be a state? The original issue was
    // wanting to modify exclude tag in the cats
    // themselves and have all the child components
    // render as a result. Is the solution to
    // set the new state from state init every
    // time the component renders?
    //
    // The question is who reads and who writes
    // to cat. Any reading from cat should be done
    // from a prop passed in from a level above (a
    // reference). However, writing to cat should
    // modify a state that is passed down to the
    // next level for reading.

    // The category that this component represents
    //let [cat, setCat] = useState(cat_init);

    const {included, enabled} = visible_status(cat, parent_exclude)    
    
    // BUG: The issue could be that included and enabled
    // are calculated after the state has been updated.
    // Perhaps they should be in a useEffect.
    
    // Whether the children of this element are hidden
    let [hidden, setHidden] = useState(true);

    // Take action when the user clicks the checkbox. Note that
    // this function cannot be called for a grayed out box,
    // because it cannot change. This means you can assume the
    // current level is enabled, meaning that none of the parents
    // are excluded.
    function handleChange() {
	toggle_cat([index], included)
    }

    // Pass requests by subcomponents up to the top level.
    // The indices argument represents the tail of the indices
    // list, and included is passed from the subcomponent
    // upwards
    function toggle_cat_sub(indices, included) {
	let new_indices = [index].concat(indices)
	toggle_cat(new_indices, included)
    }

   
    // BUG: the issue might be around here, because the top level
    // structure in cat appears to be OK -- it's just the child
    // components that are not rerendering, until a render is
    // forced (e.g. by changing hidden).
    //
    // Problem occurs when the parent is deselected while this
    // level is still ticked. If this level is unticked, and the
    // parent is deselected, than all is well.
    return <div className="category">
        <div>{cat.category} -- {cat.docs}</div>
        <Checkbox label="Include" onChange={handleChange} checked={included} enabled={enabled}/>
        <button onClick={() => setHidden(!hidden)}>Toggle Hidden</button>
        <ol> {
            cat.child.map((node,index) => {
                if (!hidden) {
                    if ("category" in node) {
                        return <li><Category index={index} cat={node} parent_exclude={!included} toggle_cat={toggle_cat_sub} /></li>
                    } else {
                        return <li><Code cat={node} parent_exclude={!included}/></li>
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

    // Function to save the codes yaml file
    function save_file() {
        invoke('save_yaml', {codeDef: code_def})
	    .then(console.log("done"))
    }
    
    // Function to get the list of groups
    function get_groups() {
        return code_def.groups
    }

    function toggle_cat(indices, included) {

	// Copy the codes definition structure
	// to modify it. This may be a performance
	// problem, but it can be optimised later.
	let code_def_copy = structuredClone(code_def);

	console.log(indices, included);
	console.log(code_def_copy)
	// Extract the cat referred to by indices
	let cat = code_def_copy;
	indices.forEach((n) => {cat = cat.child[n]})	
	
	console.log(cat)

	return 0;
	
	// Check the current state of the checkbox
	if (included) {
	    // When the current component is included,
	    // the user is wanting to disable this element,
	    // and all of its subcomponents. This involves
	    // writing an exclude tag into the current
	    // level, and clearing any exclude flags
	    // in subcomponent levels (for efficiency
	    // of representation)

	    // Deep copy the state to use setCat without
	    // problems
	    let cat_copy = Object.assign({}, cat)

	    // Clear all the nested exclude tags
	    // and then re-enable the current level
	    // exclude flag
	    cat_copy = remove_all_excludes(cat_copy)
	    cat_copy.exclude = true;

	    console.log("Excluded ", cat_copy.docs)
	    console.log(cat_copy)
	    
	    // Set the new state
	    setCat(cat_copy);
	} else {
	    // When the current component is excluded,
	    // the user is wanting to enable this level
	    // and set all the immediate subcategories
	    // to disabled by default
	    
	    // Deep copy the state to use setCat without
	    // problems
	    // BUG: problem might be here -- only the
	    // top level is deep copied, so when the
	    // functions below modify cat_copy, they
	    // are really modifying cat. If the top
	    // level of cat is not changed, and only
	    // the children are, then setCat below
	    // will not trigger a render.
	    let cat_copy = Object.assign({}, cat)

	    // Clear all the nested exclude tags
	    // and then enable the top level category,
	    // but disable all the first-level
	    // excludes in the subcategories.
	    cat_copy = remove_all_excludes(cat_copy)
	    cat_copy = set_first_excludes(cat_copy)

	    console.log("Included ", cat_copy.docs)
	    console.log(cat_copy)
	    
	    // Set the new state
	    setCat(cat_copy);
	}
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
	<button onClick={save_file}>Save as</button>
	
        <h1>ICD-10</h1>
        <div>Groups: {get_groups()}</div>
        <ol>
        <li><Category index={0} cat={code_def.child[0]} parent_exclude={false} toggle_cat = {toggle_cat}/></li>
        </ol>
    </div>
    }
}
