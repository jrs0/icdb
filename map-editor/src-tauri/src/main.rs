#![cfg_attr(
    all(not(debug_assertions), target_os = "windows"),
    windows_subsystem = "windows"
)]

use tauri_api::dialog::{select, Response};
    
#[tauri::command]
fn get_yaml() -> String {
    let result = match select(Some("yaml"), Some("~")).unwrap() {
	Response::Okay(s) => s,
	Response::OkayMultiple(_) => String::from("none"),
	Response::Cancel => String::from("Cancelled")
    };
    let f = std::fs::File::open(result).expect("Error reading file");
    let d: serde_json::Value = serde_yaml::from_reader(f).expect("Error parsing YAML");
    // let s = d.to_string();
    
    format!("{d}")
}

fn main() {

    tauri::Builder::default()
        .invoke_handler(tauri::generate_handler![get_yaml])
	.run(tauri::generate_context!())
	.expect("error while running tauri application");
}
