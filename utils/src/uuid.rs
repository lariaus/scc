use rand::distributions::DistString;

// Generate a long uuid string
pub fn generate_uuid_str() -> String {
    rand::distributions::Alphanumeric.sample_string(&mut rand::thread_rng(), 16)
    //let tmp_file_path = format!(
    //    "{}/test_{}.txt",
    //   std::env::temp_dir().to_str().unwrap(),
    // );
}

// Generate and create a temp directory.
// Return its path.
// TODO: Find an automatic way for it to get deleted.
pub fn make_temp_dir(prefix: Option<&str>) -> String {
    let prefix = prefix.unwrap_or("");
    let tmp_path = format!(
        "{}/{}{}",
        std::env::temp_dir().to_str().unwrap(),
        prefix,
        generate_uuid_str()
    );
    std::fs::create_dir_all(&tmp_path).expect("Failed to create temporary dir");
    tmp_path
}
