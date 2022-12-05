import { useState, useRef, useEffect, useMemo } from 'react';
import { invoke } from "@tauri-apps/api/tauri"
import Link from 'next/link'

import styles from '../styles/Category.module.css'

const CHECKBOX_STATES = {
    Checked: 'Checked',
    Indeterminate: 'Indeterminate',
    Empty: 'Empty',
};

function Checkbox({ checked, enabled, onChange }) {

    const checkboxRef = useRef();

    return (
        <label>
            <input
                ref={checkboxRef}
                type="checkbox"
                checked={checked}
                onChange={onChange}
            />
        </label>
    );
};

// Establish whether the component should be included
// (i.e. ticked) and whether it should be enabled
// (grayed out or not)
function visible_status(cat, group, parent_exclude) {
    // Component is included by default, unless there
    // is an exclude tag at the current level, or
    // the parent is excluded
    let exclude_tag = ("exclude" in cat) && cat.exclude.includes(group);
    let included = !exclude_tag && !parent_exclude

    // Checkbox is enabled if the parent is not excluded
    let enabled = !parent_exclude;

    return {
        included: included,
        enabled: true//enabled
    }
}

// Remove a group from the list of
// excludes in cat (modifies cat by
// reference). Think of this function
// as "unexclude_group".
function include_group(cat, group) {
    if ("exclude" in cat) {
	// The bug must be here -- not
	// removing the exclude tag
	// properly.
	// BUG: apparently group is
	// a number here, should be
	// a string
	//
        const index = cat.exclude.indexOf(group);
	console.log("Remove at", index, group)
        if (index > -1) {
            cat.exclude.splice(index, 1);
        }
    }
}


// Add a group to the list of excludes
// in cat, creating the exclude key
// if nececessary (cat is modified
// by reference)
function exclude_group(cat, group) {
    if ("exclude" in cat) {
        cat.exclude.push(group)
    } else {
        cat.exclude = [group]
    }
}


// Remove all the exclude tags in all
// sublevels of cat and return the result
function remove_all_excludes(cat, group) {

    // Remove the group from the exclude
    // list at this level
    console.log("group is", group)
    include_group(cat, group)

    if ("child" in cat) {
        // Loop over all the subcategories
        // remove the exclude
        cat.child = cat.child.map(remove_all_excludes, group)
    }

    // Return the modified category
    return (cat)
}

// Set the top-level excludes for the
// subcategories in the current category,
// and return the modified object
function set_first_excludes(cat, group) {
    if ("child" in cat) {
        cat.child = cat.child.map((subcat) => {
            // Add the group to the excludes key,
            // or create a new excludes list if
            // necessary
            exclude_group(cat, group)
            return (subcat)
        })
    }
    return (cat)
}

function Code({ index, cat, parent_exclude, toggle_cat, search_term, group }) {

    const { included, enabled } = visible_status(cat, group, parent_exclude)

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

    return <div>
        <div>
            <Checkbox onChange={handleChange}
                checked={included}
                enabled={enabled} />
            <span onClick={() => setHidden(!hidden)}>
                <span className={styles.cat_name}>{cat.code}</span>
                <span>{cat.docs}</span>
            </span>
        </div>
    </div>
}

function Category({ index, cat, parent_exclude, toggle_cat, search_term, group }) {

    const { included, enabled } = visible_status(cat, group, parent_exclude)

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

    let show = (search_term.trim().length != 0) || !hidden

    return <div>
        <div>
            <Checkbox onChange={handleChange}
                checked={included}
                enabled={enabled} />
            <span onClick={() => setHidden(!hidden)}>
                <span className={styles.cat_name}>{cat.category}</span>
                <span>{cat.docs}</span>
            </span>
        </div>
        <ol className={styles.cat_list}> {
            cat.child
                .filter((node) => {
                    //let in_title = node.category.includes(search_term);
                    let in_docs = node.docs.includes(search_term);
                    return in_docs;
                })
                .map((node, index) => {
                    if (show) {
                        if ("category" in node) {
                            return <li>
                                <Category index={index}
                                    cat={node}
                                    parent_exclude={!included}
                                    toggle_cat={toggle_cat_sub}
                                    search_term={search_term}
                                    group={group} />
                            </li>
                        } else {
                            return <li>
                                <Code index={index}
                                    cat={node}
                                    parent_exclude={!included}
                                    toggle_cat={toggle_cat_sub}
                                    search_term={search_term}
                                    group={group} />
                            </li>
                        }
                    }
                })
        } </ol>
    </div >
}

// Get the category at nesting level
// defined by indices from code_def
// structure. A reference to a
// category inside code_def is
// returned, so this function
// provides a way to modify code_def
// at arbitrary depth. Note that you
// can also use this function to get
// a subcategory relative to any
// (non-root) category, provided you
// also pass the relative indices
function get_cat(code_def, indices) {
    let cat = code_def;
    indices.forEach((n) => { cat = cat.child[n] })
    return cat;
}

export default function Home() {

    // State of the search bar
    const [searchTerm, setSearchTerm] = useState('');

    const handleSearch = event => {
        //setSearchTerm(event.target.value);
    };

    let [code_def, setCodeDef] = useState(0);

    // Function to save the codes yaml file
    function save_file() {
        invoke('save_yaml', { codeDef: code_def })
            .then(console.log("done"))
    }

    // Function to get the list of groups
    function get_groups() {
        return code_def.groups
    }

    // State for the current group
    // BUG: not starting with the correct
    // group, because it needs to be set
    // when the file is loaded.
    const [group, setGroup] = useState("");

    // Function to load the codes yaml file
    function load_file() {
        invoke('get_yaml')
            .then(JSON.parse)
            .then((res) => {
		// Note: all .then are executed
		// asynchronously, so put
		// sequential steps in here
		if ("groups" in res) {
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

    const handleGroupChange = event => {
        //console.log(event.target.value)
        setGroup(event.target.value);
    };

    function toggle_cat(indices, included) {

        // Copy the codes definition structure
        // to modify it. This may be a performance
        // problem, but it can be optimised later.
        let code_def_copy = structuredClone(code_def);

        console.log(indices, included);
        console.log(code_def_copy)

        // Extract the cat referred to by indices
        // (note that cat is modified by reference,
        // so changing the resulting cat will still
        // change code_def_copy)
        let cat = get_cat(code_def_copy, indices)

        console.log(cat)

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
	    console.log("Add here", group)
            cat = remove_all_excludes(cat, group)
            exclude_group(cat, group)

        } else {
            // When the current component is excluded,
            // the user is wanting to enable this level
            // and all sublevels, and implicitly enable
            // higher levels on the path from this node
            // to the root of the tree.

            // If the current level is excluded, then
            // either it itself has an exclude key,
            // or there is an exclude key above it.
            // Either way, there are guaranteed to be
            // not excludes below it. In addition:
            //
            // 1) If there is an exclude here, then it
            //    implies that no levels above this
            //    are excluded (otherwise this level
            //    would not be excluded)
            // 2) If there is no exclude here, then
            //    there is exactly one exclude above it
            //    (two or more would contradict the
            //    reasoning above).
            //
            // NOTE: Remember that this reasoning becomes
            // invalid if deselecting does not clear
            // all the subcategory exclude keys.

            // Find the first category above which
            // has an exclude key (which may be this
            // category).
            let indices_above = indices.slice();
            let cat_above = cat;
            while (!("exclude" in cat_above)) {
                // Move to the category above
                indices_above.pop()
                cat_above = get_cat(code_def_copy,
                    indices_above)
                console.log("above:", cat_above)
            }

            // At this point, cat is the category
            // if interest and cat_above is the
            // first higher category that contains
            // an exclude (which may be equal to cat).
            // Remove this exclude.
            delete get_cat(code_def_copy, indices_above).exclude

            // Now walk back down the tree from
            // cat above adding
            // excludes for categories not on the
            // path to cat, so as not to incorrectly
            // include any other categories. First,
            // get the indices of cat relative to
            // cat_above
            let rel_indices = indices.slice(indices_above.length);
            console.log("indices", indices)
            console.log("indices_above", indices_above)
            console.log("rel:", rel_indices)

            // Loop over all the subcategories between
            // cat_above and cat
            cat = cat_above
            rel_indices.forEach((n) => {

                // Add an exclude key to all the
                // subcategories which are not on the path
                cat.child = cat.child.map((subcat, index) => {
                    if (index != n) {
                        exclude_group(subcat, group)
                    }
                    return (subcat)
                })

                // Move down a level
                cat = cat.child[n]
            })


            //}


            //cat = remove_all_excludes(cat)

            // To enable higher levels, it is necessary to
            // remove any excludes "in the way" of
            // this category. 

            console.log("Included ", cat.docs)
            console.log(cat)
        }

        // Now save the new code_defs state
        setCodeDef(code_def_copy)
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
            <div>
                Groups: <select onChange={handleGroupChange}> {
                    get_groups().map((grp) => (
                        <option>{grp}</option>
                    ))
                } </select>
            </div>
            <ol className={styles.cat_list}>
                <li>
                    <Category index={0}
                              cat={code_def.child[0]}
                              parent_exclude={false}
                              toggle_cat={toggle_cat}
                              search_term={searchTerm}
                              group={group} />
                </li>
            </ol>
        </div >
    }
}
