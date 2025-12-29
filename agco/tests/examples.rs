use std::fs;
use std::path::Path;

#[test]
fn compile_examples() {
    let examples_dir = Path::new("../examples");
    let entries = fs::read_dir(examples_dir)
        .expect("Failed to read examples directory")
        .into_iter()
        .map(|entry| entry.expect("Failed to read entry"));
    println!("Entries: {:#?}", entries);

    for entry in entries {
        let path = entry.path();

        if path.extension().map_or(false, |ext| ext == "ag") {
            println!("Testing example: {:?}", path);
            let code = fs::read_to_string(&path).expect("Failed to read file");

            // We expect all current examples to parse successfully
            match agco::compile(&code, false) {
                Ok(_) => println!("Successfully compiled {:?}", path),
                Err(e) => panic!("Failed to compile {:?}:\n{}", path, e),
            }
        }
    }
}
