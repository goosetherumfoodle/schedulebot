#![cfg_attr(
  all(not(debug_assertions), target_os = "windows"),
  windows_subsystem = "windows"
)]

fn main() {
  tauri::Builder::default()
    .invoke_handler(tauri::generate_handler![saveStafferField, getStaffers])
    .run(tauri::generate_context!())
    .expect("error while running tauri application");
}

#[tauri::command]
fn getStaffers() -> String {
    let staffer_json = "
      [{ \"id\": \"8\", \"name\": \"Jez\", \"number\": \"412.232.5326\", \"suspendedUntil\": null  }
    , { \"id\": \"7\", \"name\": \"Big Suze\", \"number\": \"412.326.5837\", \"suspendedUntil\": null  }
    , { \"id\": \"6\", \"name\": \"Mark\", \"number\": \"412.883.9592\", \"suspendedUntil\": null  }]
";
    return (*staffer_json).to_string()
}

#[tauri::command]
fn saveStafferField(data: &str) -> String {
    println!("Received: {data}");
    format!("Hello, {}!", data)
}
