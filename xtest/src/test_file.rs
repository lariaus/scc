use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
};

// Contains all the data for a specific test in a file.
#[derive(Debug, Clone)]
pub struct TestConfig {
    label: String,
    path: String,
    output_check_mode: bool,
    expected_ret: Option<i64>,
    command: Vec<String>,
}

impl TestConfig {
    // Returns the name of the test.
    pub fn label(&self) -> &str {
        &self.label
    }

    // Returns the path to the test file.
    pub fn path(&self) -> &str {
        &self.path
    }

    // Returns true if checking the output mode is enabled.
    pub fn output_check_mode(&self) -> bool {
        self.output_check_mode
    }

    // Returns the expected return value of the test.
    pub fn expected_ret(&self) -> Option<i64> {
        self.expected_ret
    }

    // Returns the command of the test.
    pub fn command(&self) -> &[String] {
        &self.command
    }
}

// Represent an xtest file.
pub struct TestFile {
    path: String,
    tests: HashMap<String, TestConfig>,
}

impl TestFile {
    // Create a new xtestfile from path.
    pub fn new(path: &str) -> Self {
        let mut res = Self {
            path: path.to_string(),
            tests: HashMap::new(),
        };
        res._parse_file();
        res
    }

    pub fn path(&self) -> &str {
        &self.path
    }

    pub fn tests<'a>(&'a self) -> impl Iterator<Item = &'a TestConfig> {
        self.tests.iter().map(|p| p.1)
    }

    fn _parse_file(&mut self) {
        let reader = BufReader::new(
            File::open(&self.path)
                .expect(&format!("Failed to open XTEST test file `{}`", &self.path)),
        );
        let mut current_test = None;
        for line in reader.lines() {
            let line = line.expect("Failed to read XTEST test file");
            if line.starts_with("// XTEST:") {
                let cmd = &line[9..];
                self._handle_xtest_directive(&mut current_test, cmd);
            } else if line.starts_with("// XTEST-OUTPUT-CHECK") {
                self._handle_output_check_directive(&mut current_test);
            }
        }

        self._resolve_current_test(&mut current_test);
    }

    fn _resolve_current_test(&mut self, current_test: &mut Option<TestConfig>) {
        // Nothing to add.
        if current_test.is_none() {
            return;
        }

        // Swap it with None.
        let mut test = None;
        std::mem::swap(&mut test, current_test);
        let test = test.unwrap();

        // Add the test.
        let label = test.label.clone();
        assert!(
            !self.tests.contains_key(&label),
            "Multiple definitions of test `{}`",
            label
        );

        self.tests.insert(label, test);
    }

    fn _handle_xtest_directive(&mut self, current_test: &mut Option<TestConfig>, cmd: &str) {
        self._resolve_current_test(current_test);

        let mut cmd: Vec<_> = cmd
            .split_ascii_whitespace()
            .into_iter()
            .map(|x| x.to_string())
            .collect();
        assert!(!cmd.is_empty());

        let label = if cmd.first().unwrap().starts_with("@") {
            cmd.remove(0)[1..].to_string()
        } else {
            "main".to_string()
        };

        *current_test = Some(TestConfig {
            label: label,
            path: self.path.clone(),
            output_check_mode: false,
            expected_ret: None,
            command: cmd,
        });
    }

    fn _handle_output_check_directive(&mut self, current_test: &mut Option<TestConfig>) {
        let current_test = current_test
            .as_mut()
            .expect("Can't use XTEST-OUTPUT-CHECK without a test");
        current_test.output_check_mode = true;
    }
}

#[cfg(test)]
mod tests {

    use std::{fs::File, io::Write};

    use utils::uuid;

    use super::*;

    fn get_temp_file(s: &str) -> String {
        let tmp_dir = uuid::make_temp_dir(Some("check_xtest_"));
        let tmp_path = format!("{}/test.txt", tmp_dir);
        let mut file = File::create_new(&tmp_path).unwrap();
        file.write_all(s.as_bytes()).unwrap();
        tmp_path
    }

    #[test]
    fn basic_multiple_tests() {
        let tst_path = get_temp_file(
            r#"
// XTEST: @test0 runv1 foo bar
// XTEST: @hello hallo 3 4 5
foo
// XTEST: @123 none

                "#,
        );
        let tst_file = TestFile::new(&tst_path);
        assert_eq!(tst_file.tests.len(), 3);

        let test0 = tst_file.tests.get("test0").unwrap();
        assert_eq!(test0.command(), vec!["runv1", "foo", "bar"]);

        let test1 = tst_file.tests.get("hello").unwrap();
        assert_eq!(test1.command(), vec!["hallo", "3", "4", "5"]);

        let test2 = tst_file.tests.get("123").unwrap();
        assert_eq!(test2.command(), vec!["none"]);
    }

    #[test]
    fn multiple_tests_with_opts() {
        let tst_path = get_temp_file(
            r#"
// XTEST: @test0 runv1 foo bar
// XTEST-OUTPUT-CHECK

// XTEST: @hello hallo 3 4 5

// XTEST: @123 none
// XTEST-OUTPUT-CHECK

                "#,
        );
        let tst_file = TestFile::new(&tst_path);
        assert_eq!(tst_file.tests.len(), 3);

        let test0 = tst_file.tests.get("test0").unwrap();
        assert_eq!(test0.command(), vec!["runv1", "foo", "bar"]);
        assert!(test0.output_check_mode());

        let test1 = tst_file.tests.get("hello").unwrap();
        assert_eq!(test1.command(), vec!["hallo", "3", "4", "5"]);
        assert!(!test1.output_check_mode());

        let test2 = tst_file.tests.get("123").unwrap();
        assert_eq!(test2.command(), vec!["none"]);
        assert!(test2.output_check_mode());
    }
}
