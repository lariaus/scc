// Struct class used to run output_check

use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader},
};

use regex::Regex;

#[derive(Debug, Clone)]
enum TestCommand {
    Check(String),
    CheckNext(String),
    CheckSame(String),
    CheckNot(String),
}

#[derive(Debug, Clone)]
struct TestMatch {
    cmd: TestCommand,
    cmd_line: String,
    cmd_line_idx: usize,
    // (line_pos, col_start, col_end)
    match_pos: Option<(usize, usize, usize)>,
}

impl TestMatch {
    fn is_check(&self) -> bool {
        match &self.cmd {
            TestCommand::Check(_) => true,
            _ => false,
        }
    }

    fn is_check_next(&self) -> bool {
        match &self.cmd {
            TestCommand::CheckNext(_) => true,
            _ => false,
        }
    }

    fn is_check_same(&self) -> bool {
        match &self.cmd {
            TestCommand::CheckSame(_) => true,
            _ => false,
        }
    }

    fn is_check_not(&self) -> bool {
        match &self.cmd {
            TestCommand::CheckNot(_) => true,
            _ => false,
        }
    }
}

fn convert_pattern_to_regex(
    pattern: &str,
    vars_map: &HashMap<String, String>,
    var_defs: &mut Vec<String>,
) -> String {
    // let re = Regex::new(r"({{.*}}").unwrap();

    let mut re_pattern = String::new();
    let mut inside_pattern = false;
    let mut inside_varref = false;
    let mut inside_vardef = false;
    let mut var_name = String::new();

    let mut it = pattern.chars().peekable();
    let to_escape_chars = &['[', ']', '{', '}', '(', ')', '|', '.', '*', '+', '?'];
    loop {
        let c = match it.next() {
            Some(c) => c,
            None => break,
        };

        let in_raw = !inside_pattern && !inside_vardef && !inside_varref;

        if in_raw && c == '{' && it.peek().cloned() == Some('{') {
            inside_pattern = true;
            it.next();
        } else if in_raw && c == '[' && it.peek().cloned() == Some('[') {
            inside_varref = true;
            it.next();
        } else if inside_pattern && c == '}' && it.peek().cloned() == Some('}') {
            inside_pattern = false;
            it.next();
        } else if inside_varref && c == ']' && it.peek().cloned() == Some(']') {
            inside_varref = false;
            it.next();
            let val = vars_map.get(&var_name).expect(&format!(
                "Usage of undefined variable `{}` in pattern `{}`",
                &var_name, pattern
            ));
            // TODO: Maybe next to go through all chars and escape the ones needed.
            re_pattern.push_str(val);
            var_name.clear();
        } else if inside_vardef && c == ']' && it.peek().cloned() == Some(']') {
            inside_vardef = false;
            it.next();
            assert!(
                !vars_map.contains_key(&var_name),
                "Redefinition of variable `{}` in pattern `{}`",
                &var_name,
                pattern
            );
            re_pattern.push(')');
            let mut tmp = String::new();
            std::mem::swap(&mut tmp, &mut var_name);
            var_defs.push(tmp);
        } else if inside_varref && c == ':' {
            // Switch from varref to vardef
            assert!(!var_name.is_empty());
            inside_varref = false;
            inside_vardef = true;
            re_pattern.push('(');
        } else if inside_varref {
            var_name.push(c);
        } else if in_raw && to_escape_chars.contains(&c) {
            re_pattern.push('\\');
            re_pattern.push(c);
        } else {
            re_pattern.push(c);
        }
    }

    // println!("re_pattern = /{}/", re_pattern);
    re_pattern
}

fn try_find_match(
    vars_map: &mut HashMap<String, String>,
    test_match: &mut TestMatch,
    line: &str,
    line_offset: usize,
    line_idx: usize,
) -> bool {
    let line = &line[line_offset..];
    let pattern = match &test_match.cmd {
        TestCommand::Check(p) => p,
        TestCommand::CheckNext(p) => p,
        TestCommand::CheckSame(p) => p,
        TestCommand::CheckNot(p) => p,
    };
    let mut var_defs = Vec::new();
    let re_pattern = convert_pattern_to_regex(pattern, vars_map, &mut var_defs);
    let re = Regex::new(&re_pattern).expect(&format!(
        "Failed to create regex from `{}` (original: `{}`)",
        re_pattern, pattern
    ));

    let caps = match re.captures(line) {
        Some(caps) => caps,
        None => return false,
    };

    // Extract the position from the first match.
    let full_match = caps.get(0).unwrap();
    test_match.match_pos = Some((
        line_idx,
        line_offset + full_match.start(),
        line_offset + full_match.start() + full_match.len(),
    ));

    // Extract the variables from the other matches.
    for (idx, var_name) in var_defs.into_iter().enumerate() {
        let var_match = caps.get(idx + 1).unwrap();
        // println!("define var `{}` = `{}`", &var_name, var_match.as_str());
        vars_map.insert(var_name, var_match.as_str().to_string());
    }

    true
}

pub struct OutputCheckOpts {
    pub panic_on_failure: bool,
}

impl OutputCheckOpts {
    pub fn new() -> Self {
        Self {
            panic_on_failure: false,
        }
    }
}

struct OutputCheckerImpl {
    opts: OutputCheckOpts,
    matches: Vec<TestMatch>,
    output_lines: Vec<String>,
    test_file_path: String,
    vars_map: HashMap<String, String>,
}

impl OutputCheckerImpl {
    fn new(opts: OutputCheckOpts) -> Self {
        Self {
            opts,
            matches: vec![],
            output_lines: vec![],
            test_file_path: String::new(),
            vars_map: HashMap::new(),
        }
    }

    // Find all test commands from the test file
    fn _find_commands(&mut self, test_file_path: &str, _test_label: &str) {
        self.test_file_path = test_file_path.to_string();

        let reader = BufReader::new(
            File::open(test_file_path)
                .expect(&format!("Failed to open test file `{}`", test_file_path)),
        );
        let mut line_idx = 0;
        for full_line in reader.lines() {
            line_idx += 1;
            let full_line = full_line.unwrap();
            let line = full_line.trim_ascii_start();
            let line = if line.starts_with("//") {
                line[2..].trim_ascii_start()
            } else {
                line
            };
            if line.is_empty() {
                continue;
            }

            // CHECK: <pattern>
            if line.starts_with("CHECK:") {
                let pattern = line[6..].trim_ascii();
                self.matches.push(TestMatch {
                    cmd: TestCommand::Check(pattern.to_string()),
                    cmd_line: full_line,
                    cmd_line_idx: line_idx,
                    match_pos: None,
                });
            }
            // CHECK-NEXT: <pattern>
            else if line.starts_with("CHECK-NEXT:") {
                let pattern = line[11..].trim_ascii();
                self.matches.push(TestMatch {
                    cmd: TestCommand::CheckNext(pattern.to_string()),
                    cmd_line: full_line,
                    cmd_line_idx: line_idx,
                    match_pos: None,
                });
            }
            // CHECK-SAME: <pattern>
            else if line.starts_with("CHECK-SAME:") {
                let pattern = line[11..].trim_ascii();
                self.matches.push(TestMatch {
                    cmd: TestCommand::CheckSame(pattern.to_string()),
                    cmd_line: full_line,
                    cmd_line_idx: line_idx,
                    match_pos: None,
                });
            }
            // CHECK-NOT: <pattern>
            else if line.starts_with("CHECK-NOT:") {
                let pattern = line[10..].trim_ascii();
                self.matches.push(TestMatch {
                    cmd: TestCommand::CheckNot(pattern.to_string()),
                    cmd_line: full_line,
                    cmd_line_idx: line_idx,
                    match_pos: None,
                });
            }
        }

        assert!(
            self.matches.len() > 0,
            "No CHECK command found in `{}`",
            self.test_file_path
        );
    }

    // Load the output lines from a string.
    fn _load_output_str(&mut self, full_test_output: &str) {
        self.output_lines = full_test_output
            .split("\n")
            .map(|x| x.to_string())
            .collect();
    }

    fn _check_impl(&mut self) -> Option<(TestMatch, usize)> {
        let mut prev_match: Option<(usize, usize, usize)> = None;

        for test_match in &mut self.matches {
            // Ignore check-not in the first pass.
            if test_match.is_check_not() {
                continue;
            }

            if test_match.is_check() {
                let mut current_line_idx = match prev_match {
                    Some(pos) => pos.0 + 1,
                    None => 0,
                };
                let start_line = if current_line_idx >= self.output_lines.len() {
                    self.output_lines.len() - 1
                } else {
                    current_line_idx
                };

                // CHECK command: keep matching until going to the same line
                loop {
                    // Reach EOF: Return match error.
                    if current_line_idx >= self.output_lines.len() {
                        return Some((test_match.clone(), start_line));
                    }

                    let line = &self.output_lines[current_line_idx];
                    if try_find_match(&mut self.vars_map, test_match, line, 0, current_line_idx) {
                        // Match found
                        prev_match = test_match.match_pos;
                        break;
                    }

                    current_line_idx += 1;
                }
            } else if test_match.is_check_next() || test_match.is_check_same() {
                let (line_idx, line_offset) = match prev_match {
                    Some(pos) => {
                        if test_match.is_check_next() {
                            (pos.0 + 1, 0)
                        } else {
                            (pos.0, pos.2)
                        }
                    }
                    None => (0, 0),
                };

                // Match failure.
                if line_idx >= self.output_lines.len()
                    || !try_find_match(
                        &mut self.vars_map,
                        test_match,
                        &self.output_lines[line_idx],
                        line_offset,
                        line_idx,
                    )
                {
                    return Some((test_match.clone(), line_idx));
                }

                // match found.
                prev_match = test_match.match_pos;
            } else {
                unreachable!();
            }
        }

        // Look for check-not in the second match
        for i in 0..self.matches.len() {
            if !self.matches[i].is_check_not() {
                continue;
            }

            // Find the interval to search for
            let mut i_beg = (i as isize) - 1;
            let first_line_idx = loop {
                if i_beg < 0 {
                    break 0;
                } else if !self.matches[i_beg as usize].is_check_not() {
                    break self.matches[i_beg as usize].match_pos.unwrap().0 + 1;
                } else {
                    i_beg -= 1;
                }
            };
            let mut i_end = i + 1;
            let mut unmatchable = false;
            let last_line_idx = loop {
                if i_end == self.matches.len() {
                    break self.output_lines.len() - 1;
                } else if !self.matches[i_end].is_check_not() {
                    let next_pos = self.matches[i_end].match_pos.unwrap().0;
                    if next_pos == 0 {
                        // Special case: The first CHECK match is on the first line.
                        // But there is CHECK-NOT before.
                        // It'll never match
                        unmatchable = true;
                        break 0;
                    }
                    break next_pos - 1;
                } else {
                    i_end += 1;
                }
            };
            if unmatchable {
                continue;
            }

            let test_match = &mut self.matches[i];

            // Search for a match in the interval
            for line_idx in first_line_idx..last_line_idx + 1 {
                // Match success is a failure.
                if try_find_match(
                    &mut self.vars_map,
                    test_match,
                    &self.output_lines[line_idx],
                    0,
                    line_idx,
                ) {
                    return Some((test_match.clone(), first_line_idx));
                }
            }
        }

        None
    }

    fn check(&mut self) -> bool {
        let err = match self._check_impl() {
            Some(err) => err,
            None => return true,
        };

        let (test_match, line_idx) = err;
        let line_idx = if line_idx >= self.output_lines.len() {
            self.output_lines.len() - 1
        } else {
            line_idx
        };

        let err_msg = format!(
            "Match failure at {}:{}: {}\nStart looking from output line #{}:\n{}",
            self.test_file_path,
            test_match.cmd_line_idx,
            test_match.cmd_line,
            line_idx + 1,
            self.output_lines[line_idx]
        );

        if self.opts.panic_on_failure {
            panic!("{}", err_msg);
        } else {
            println!("{}", err_msg);
        }
        false
    }
}

fn prepare_outputcheck(
    opts: OutputCheckOpts,
    test_file_path: &str,
    test_label: &str,
    full_test_output: &str,
) -> OutputCheckerImpl {
    let mut checker = OutputCheckerImpl::new(opts);

    // Find the test commands (CHECK-XXX).
    checker._find_commands(test_file_path, test_label);

    // Load the output data
    checker._load_output_str(full_test_output);

    checker
}

// Check the test output
pub fn run_outputcheck(
    opts: OutputCheckOpts,
    test_file_path: &str,
    test_label: &str,
    full_test_output: &str,
) -> bool {
    let mut checker = prepare_outputcheck(opts, test_file_path, test_label, full_test_output);
    checker.check()
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
    fn basic_check() {
        let tst_path = get_temp_file(
            r#"
// CHECK: foo
// CHECK: bar
// CHECK: oops
"#,
        );
        let full_test_output = r#"
bro
tt
 nn foo
 xbar
 fun
coucoopsxl
        "#;
        let mut checker =
            prepare_outputcheck(OutputCheckOpts::new(), &tst_path, "main", full_test_output);
        assert!(checker.check());

        assert_eq!(checker.matches.len(), 3);
        assert_eq!(checker.matches[0].cmd_line_idx, 2); // 1-indexed
        assert_eq!(checker.matches[0].match_pos, Some((3, 4, 7))); // 0-indexex

        assert_eq!(checker.matches[1].cmd_line_idx, 3); // 1-indexed
        assert_eq!(checker.matches[1].match_pos, Some((4, 2, 5))); // 0-indexex

        assert_eq!(checker.matches[2].cmd_line_idx, 4); // 1-indexed
        assert_eq!(checker.matches[2].match_pos, Some((6, 4, 8))); // 0-indexex
    }

    #[test]
    fn basic_check_fail() {
        let tst_path = get_temp_file(
            r#"
// CHECK: foo
// CHECK: bar
// CHECK: oops
"#,
        );
        let full_test_output = r#"
bro
tt
 nn foo
 xbaxr
 fun
coucoopsxl
        "#;
        let mut checker =
            prepare_outputcheck(OutputCheckOpts::new(), &tst_path, "main", full_test_output);
        assert!(!checker.check());

        assert_eq!(checker.matches.len(), 3);
        assert_eq!(checker.matches[0].cmd_line_idx, 2); // 1-indexed
        assert_eq!(checker.matches[0].match_pos, Some((3, 4, 7))); // 0-indexex

        assert_eq!(checker.matches[1].cmd_line_idx, 3); // 1-indexed
        assert_eq!(checker.matches[1].match_pos, None); // 0-indexex

        assert_eq!(checker.matches[2].cmd_line_idx, 4); // 1-indexed
        assert_eq!(checker.matches[2].match_pos, None); // 0-indexex
    }

    #[test]
    fn basic_check_next() {
        let tst_path = get_temp_file(
            r#"
// CHECK-NEXT: foo
// CHECK-NEXT: bar
// CHECK-NEXT: oops
"#,
        );
        let full_test_output = r#" nn foo
 xbar
coucoopsxl
        "#;
        let mut checker =
            prepare_outputcheck(OutputCheckOpts::new(), &tst_path, "main", full_test_output);
        assert!(checker.check());

        assert_eq!(checker.matches.len(), 3);
        assert_eq!(checker.matches[0].cmd_line_idx, 2); // 1-indexed
        assert_eq!(checker.matches[0].match_pos, Some((0, 4, 7))); // 0-indexex

        assert_eq!(checker.matches[1].cmd_line_idx, 3); // 1-indexed
        assert_eq!(checker.matches[1].match_pos, Some((1, 2, 5))); // 0-indexex

        assert_eq!(checker.matches[2].cmd_line_idx, 4); // 1-indexed
        assert_eq!(checker.matches[2].match_pos, Some((2, 4, 8))); // 0-indexex
    }

    #[test]
    fn basic_check_next_fail() {
        let tst_path = get_temp_file(
            r#"
// CHECK-NEXT: foo
// CHECK-NEXT: bar
// CHECK-NEXT: oops
"#,
        );
        let full_test_output = r#" nn foo
 xbar

coucoopsxl
        "#;
        let mut checker =
            prepare_outputcheck(OutputCheckOpts::new(), &tst_path, "main", full_test_output);
        assert!(!checker.check());

        assert_eq!(checker.matches.len(), 3);
        assert_eq!(checker.matches[0].cmd_line_idx, 2); // 1-indexed
        assert_eq!(checker.matches[0].match_pos, Some((0, 4, 7))); // 0-indexex

        assert_eq!(checker.matches[1].cmd_line_idx, 3); // 1-indexed
        assert_eq!(checker.matches[1].match_pos, Some((1, 2, 5))); // 0-indexex

        assert_eq!(checker.matches[2].cmd_line_idx, 4); // 1-indexed
        assert_eq!(checker.matches[2].match_pos, None); // 0-indexex
    }

    #[test]
    fn basic_check_same() {
        let tst_path = get_temp_file(
            r#"
// CHECK: foo
// CHECK-SAME: bar
// CHECK-SAME: baz
"#,
        );
        let full_test_output = "bazfoobar";
        let mut checker =
            prepare_outputcheck(OutputCheckOpts::new(), &tst_path, "main", full_test_output);
        assert!(!checker.check());

        assert_eq!(checker.matches.len(), 3);
        assert_eq!(checker.matches[0].cmd_line_idx, 2); // 1-indexed
        assert_eq!(checker.matches[0].match_pos, Some((0, 3, 6))); // 0-indexex

        assert_eq!(checker.matches[1].cmd_line_idx, 3); // 1-indexed
        assert_eq!(checker.matches[1].match_pos, Some((0, 6, 9))); // 0-indexex

        assert_eq!(checker.matches[2].cmd_line_idx, 4); // 1-indexed
        assert_eq!(checker.matches[2].match_pos, None); // 0-indexex
    }

    #[test]
    fn basic_check_not() {
        let tst_path = get_temp_file(
            r#"
// CHECK: foo
// CHECK-NOT: baz
// CHECK: bar
"#,
        );
        let full_test_output = r#" 
nn foo baz
 xbar
 bazel
coucou
        "#;
        let mut checker =
            prepare_outputcheck(OutputCheckOpts::new(), &tst_path, "main", full_test_output);
        assert!(checker.check());

        assert_eq!(checker.matches.len(), 3);
        assert_eq!(checker.matches[0].cmd_line_idx, 2); // 1-indexed
        assert_eq!(checker.matches[0].match_pos, Some((1, 3, 6))); // 0-indexex

        assert_eq!(checker.matches[1].cmd_line_idx, 3); // 1-indexed
        assert_eq!(checker.matches[1].match_pos, None); // 0-indexex

        assert_eq!(checker.matches[2].cmd_line_idx, 4); // 1-indexed
        assert_eq!(checker.matches[2].match_pos, Some((2, 2, 5))); // 0-indexex
    }

    #[test]
    fn basic_check_not_fail() {
        let tst_path = get_temp_file(
            r#"
// CHECK: foo
// CHECK-NOT: baz
// CHECK: bar
"#,
        );
        let full_test_output = r#" 
nn foo baz
 bazel
 xbar
coucou
        "#;
        let mut checker =
            prepare_outputcheck(OutputCheckOpts::new(), &tst_path, "main", full_test_output);
        assert!(!checker.check());

        assert_eq!(checker.matches.len(), 3);
        assert_eq!(checker.matches[0].cmd_line_idx, 2); // 1-indexed
        assert_eq!(checker.matches[0].match_pos, Some((1, 3, 6))); // 0-indexex

        assert_eq!(checker.matches[1].cmd_line_idx, 3); // 1-indexed
        assert_eq!(checker.matches[1].match_pos, Some((2, 1, 4))); // 0-indexex

        assert_eq!(checker.matches[2].cmd_line_idx, 4); // 1-indexed
        assert_eq!(checker.matches[2].match_pos, Some((3, 2, 5))); // 0-indexex
    }

    #[test]
    fn basic_check_regex() {
        let tst_path = get_temp_file(
            r#"
// CHECK: foo{{.*}}bar
// CHECK: bop[]
// CHECK: 2 3 4
// CHECK: [[V0:.*]]= {{.*}}
// CHECK: echo |[[V0]]|
"#,
        );
        let full_test_output = r#"
foox45bar
45bop[]12
12 3 45
my_var= 128
run echo |my_var|--
        "#;
        let mut checker =
            prepare_outputcheck(OutputCheckOpts::new(), &tst_path, "main", full_test_output);
        assert!(checker.check());
        assert_eq!(checker.matches.len(), 5);

        assert_eq!(checker.matches[0].cmd_line_idx, 2); // 1-indexed
        assert_eq!(checker.matches[0].match_pos, Some((1, 0, 9))); // 0-indexed

        assert_eq!(checker.matches[1].cmd_line_idx, 3); // 1-indexed
        assert_eq!(checker.matches[1].match_pos, Some((2, 2, 7))); // 0-indexed

        assert_eq!(checker.matches[2].cmd_line_idx, 4); // 1-indexed
        assert_eq!(checker.matches[2].match_pos, Some((3, 1, 6))); // 0-indexed

        assert_eq!(checker.matches[3].cmd_line_idx, 5); // 1-indexed
        assert_eq!(checker.matches[3].match_pos, Some((4, 0, 11))); // 0-indexed

        assert_eq!(checker.matches[4].cmd_line_idx, 6); // 1-indexed
        assert_eq!(checker.matches[4].match_pos, Some((5, 4, 17))); // 0-indexed
    }
}
