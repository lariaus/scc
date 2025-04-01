use std::collections::HashMap;

use crate::{
    outputcheck::{run_outputcheck, OutputCheckOpts},
    test_file::{TestConfig, TestFile},
};

// A runner for a specific test.
pub trait TestRunner {
    // Returns the name of the runner. Used to match with the command name.
    fn get_runner_name(&self) -> &'static str;

    // Runs the actual test, write it's output to `us`, and returns the result.
    fn run_test(&self, cfg: &TestConfig, os: &mut OutputTestWriter) -> i64;
}

// This is the class responsible for running tests.
pub struct TestDriver {
    runners: HashMap<String, Box<dyn TestRunner>>,
}

// Wrapper class implementing a basic writer, used to capture the test output.
pub struct OutputTestWriter {
    has_output: bool,
    output_str: Option<String>,
}

impl OutputTestWriter {
    fn new(capture_ouput: bool) -> Self {
        Self {
            has_output: false,
            output_str: if capture_ouput {
                Some("".to_string())
            } else {
                None
            },
        }
    }
}

impl std::io::Write for OutputTestWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if buf.is_empty() {
            return Ok(0);
        }

        let str = std::str::from_utf8(buf).unwrap();
        self.has_output = true;
        print!("{}", str);
        if let Some(out_str) = self.output_str.as_mut() {
            out_str.push_str(str);
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl TestDriver {
    // Create a new driver used to run the tests.
    pub fn new() -> Self {
        Self {
            runners: HashMap::new(),
        }
    }

    // Add a new special runner for the test.
    pub fn add_runner<T: TestRunner + Default + 'static>(&mut self) {
        let runner = Box::new(T::default());
        let name = runner.get_runner_name();
        self.runners.insert(name.to_string(), runner);
    }

    // Run all tests.
    fn _exec_test(&self, tst: TestFile) -> TestFileResults {
        let mut res = TestFileResults {
            results: HashMap::new(),
        };
        for test_cfg in tst.tests() {
            let test_res = self._exec_single_test(test_cfg.clone());
            res.results
                .insert(test_res.cfg.label().to_string(), test_res);
        }
        res
    }

    // Run a single test.
    fn _exec_single_test(&self, cfg: TestConfig) -> TestResult {
        use std::io::Write;

        let mut res = TestResult {
            cfg,
            ret_value: 0,
            empty_output: false,
            output: None,
        };

        // Get the runner
        let cmd_name = &res.cfg.command()[0];
        let runner = self
            .runners
            .get(cmd_name)
            .expect(&format!("No runner found for command `{}`", cmd_name));

        // Exec the thest
        let mut os = OutputTestWriter::new(res.cfg.output_check_mode());
        os.flush().unwrap();
        let ret_value = runner.run_test(&res.cfg, &mut os);

        res.ret_value = ret_value;
        res.empty_output = !os.has_output;
        res.output = os.output_str;
        res
    }

    fn _check_single_test(&self, tst: &TestResult) {
        println!("Checking test {} @{}", tst.cfg.path(), tst.cfg.label());
        if let Some(expected_ret) = tst.cfg.expected_ret() {
            assert_eq!(tst.ret_value, expected_ret, "Invalid output return value");
        }
        if tst.cfg.output_check_mode() {
            let opts = OutputCheckOpts {
                panic_on_failure: true,
            };
            run_outputcheck(
                opts,
                tst.cfg.path(),
                tst.cfg.label(),
                tst.output.as_ref().unwrap(),
            );
        }
    }

    // Run a single test
    pub fn run_test(&self, path: &str) {
        let tst = TestFile::new(path);
        assert!(
            tst.tests().next().is_some(),
            "No XTEST directive found in `{}`",
            path
        );

        let results = self._exec_test(tst);

        for tst_result in results.results.values() {
            self._check_single_test(tst_result);
        }
    }
}

// Rest of a single test.
struct TestResult {
    cfg: TestConfig,
    ret_value: i64,
    empty_output: bool,
    output: Option<String>,
}

// Results of all tests
struct TestFileResults {
    results: HashMap<String, TestResult>,
}

#[cfg(test)]
mod tests {

    use std::{
        fs::File,
        io::{BufRead, BufReader, Write},
    };

    use utils::uuid;

    use super::*;

    #[derive(Debug, Default)]
    struct RunnerV1 {}

    impl TestRunner for RunnerV1 {
        fn get_runner_name(&self) -> &'static str {
            "runv1"
        }

        fn run_test(&self, cfg: &TestConfig, os: &mut OutputTestWriter) -> i64 {
            let reader = BufReader::new(File::open(cfg.path()).unwrap());

            let mut nums = Vec::new();

            for line in reader.lines() {
                let line = line.unwrap();
                if line.starts_with("- ") {
                    let line = &line[2..];
                    let num = match line.parse::<i64>() {
                        Ok(val) => val,
                        Err(_) => {
                            write!(os, "Failed to parse line `{}`\n", line).unwrap();
                            return 1;
                        }
                    };
                    nums.push(num);
                }
            }

            if nums.len() != 3 {
                write!(os, "Expected 3 nums but got {}\n", nums.len()).unwrap();
                return 1;
            }

            if nums[0] + nums[1] != nums[2] {
                write!(
                    os,
                    "Expected output to be {} but got {}\n",
                    nums[0] + nums[1],
                    nums[2]
                )
                .unwrap();
                return 1;
            }

            write!(os, "{} + {} = {}\n", nums[0], nums[1], nums[2]).unwrap();
            return 0;
        }
    }

    fn get_driver() -> TestDriver {
        let mut driver = TestDriver::new();
        driver.add_runner::<RunnerV1>();
        driver
    }

    fn get_temp_file(s: &str) -> String {
        let tmp_dir = uuid::make_temp_dir(Some("check_xtest_"));
        let tmp_path = format!("{}/test.txt", tmp_dir);
        let mut file = File::create_new(&tmp_path).unwrap();
        file.write_all(s.as_bytes()).unwrap();
        tmp_path
    }

    #[test]
    fn test_runv1_manual() {
        let driver = get_driver();
        let tst_path = get_temp_file(
            r#"
// XTEST: runv1
- 3
- 8
- 11
        "#,
        );
        let results = driver._exec_test(TestFile::new(&tst_path)).results;
        assert_eq!(results.len(), 1);
        let res = results.iter().next().unwrap().1;
        assert_eq!(res.cfg.label(), "main");
        assert_eq!(res.ret_value, 0);
    }

    #[test]
    fn test_runv1() {
        let driver = get_driver();
        let tst_path = get_temp_file(
            r#"
// XTEST: runv1
- 3
- 8
- 11
        "#,
        );
        driver.run_test(&tst_path);
    }

    #[test]
    #[should_panic(expected = "No CHECK command found in")]
    fn test_runv1_outputcheck_empty() {
        let driver = get_driver();
        let tst_path = get_temp_file(
            r#"
// XTEST: runv1
// XTEST-OUTPUT-CHECK
- 3
- 8
- 11
        "#,
        );
        driver.run_test(&tst_path);
    }

    #[test]
    fn test_runv1_outputcheck() {
        let driver = get_driver();
        let tst_path = get_temp_file(
            r#"
// XTEST: runv1
// XTEST-OUTPUT-CHECK
// CHECK: 3 + 8 = 11
- 3
- 8
- 11
        "#,
        );
        driver.run_test(&tst_path);
    }

    #[test]
    #[should_panic(expected = ": // CHECK: 3 + 8 = 10")]
    fn test_runv1_outputcheck_fail() {
        let driver = get_driver();
        let tst_path = get_temp_file(
            r#"
// XTEST: runv1
// XTEST-OUTPUT-CHECK
// CHECK: 3 + 8 = 10
- 3
- 8
- 11
        "#,
        );
        driver.run_test(&tst_path);
    }
}
