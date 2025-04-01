use std::{collections::HashMap, process::exit};

// Represent an action to run when when an argument is found.
enum ArgAction {
    RunWithoutArg(Option<Box<dyn FnOnce() -> ()>>),
    RunWithArg(Option<Box<dyn FnOnce(String) -> ()>>),
    SetFlag,
    SetOptionValue,
    SpecialPrintUsage,
    SpecialPrintVersion,
}

// Information about a specific argument,
struct CommandArg {
    short_arg: Option<char>,
    long_arg: String,
    description: String,
    required: bool,
    action: ArgAction,
    vals: Vec<Option<String>>,
    allow_multiple_vals: bool,
}

// Represent an argument parsed by the CLI.
// This is useful to go through parsed arguments in order.
pub struct ParsedArgument<'a> {
    long_arg: &'a str,
    val: Option<&'a str>,
}

impl<'a> ParsedArgument<'a> {
    pub fn argname(&self) -> &'a str {
        self.long_arg
    }

    pub fn value(&self) -> Option<&'a str> {
        self.val
    }
}

// Helper class to parse CLI arguments.
// There are already many crates with macros really powerful for this.
// This is a lot simpler, but allows for having other parts of the code extend the argument instead of having them all defined in one place.
// This is needed for SIR where many parts of the code will define different arguments.
// Actions triggers callback.
pub struct ArgumentsParser {
    bin_name: String,
    description: String,
    version: String,
    args: Vec<CommandArg>,
    short_args_map: HashMap<char, usize>,
    long_args_map: HashMap<String, usize>,
    parsed: bool,
    parsed_args: Vec<(usize, usize)>,
}

impl ArgumentsParser {
    // Create the argument parser
    pub fn new(bin_name: String, description: String, version: String) -> Self {
        let mut res = Self {
            bin_name,
            description,
            version,
            args: Vec::new(),
            short_args_map: HashMap::new(),
            long_args_map: HashMap::new(),
            parsed: false,
            parsed_args: Vec::new(),
        };
        res._add_default_arguments();
        res
    }

    // Add a new argument which is just a flag (set or not).
    // Can query it after parsing using `get_flag`.
    pub fn add_flag(
        &mut self,
        long_arg: &str,
        short_arg: Option<char>,
        allow_multiple: bool,
        description: &str,
    ) {
        self._add_arg(CommandArg {
            short_arg: short_arg,
            long_arg: long_arg.to_owned(),
            description: description.to_owned(),
            required: false,
            action: ArgAction::SetFlag,
            vals: vec![],
            allow_multiple_vals: allow_multiple,
        });
    }

    // Add an an option which must have a value.
    // Can query it after parsing using `get_option_value`.
    pub fn add_option(
        &mut self,
        long_arg: &str,
        short_arg: Option<char>,
        required: bool,
        allow_multiple: bool,
        description: &str,
    ) {
        self._add_arg(CommandArg {
            short_arg: short_arg,
            long_arg: long_arg.to_owned(),
            description: description.to_owned(),
            required,
            action: ArgAction::SetOptionValue,
            vals: vec![],
            allow_multiple_vals: allow_multiple,
        });
    }

    // Add a new argument which will trigger a callback function when set.
    pub fn add_option_with_callback<F: FnOnce() -> () + 'static>(
        &mut self,
        long_arg: &str,
        short_arg: Option<char>,
        required: bool,
        allow_multiple: bool,
        description: &str,
        callback: F,
    ) {
        self._add_arg(CommandArg {
            short_arg: short_arg,
            long_arg: long_arg.to_owned(),
            description: description.to_owned(),
            required,
            action: ArgAction::RunWithoutArg(Some(Box::new(callback))),
            vals: vec![],
            allow_multiple_vals: allow_multiple,
        });
    }

    // Add a new argument which will trigger a callback function when set.
    pub fn add_valued_option_with_callback<F: FnOnce(String) -> () + 'static>(
        &mut self,
        long_arg: &str,
        short_arg: Option<char>,
        required: bool,
        allow_multiple: bool,
        description: &str,
        callback: F,
    ) {
        self._add_arg(CommandArg {
            short_arg: short_arg,
            long_arg: long_arg.to_owned(),
            description: description.to_owned(),
            required,
            action: ArgAction::RunWithArg(Some(Box::new(callback))),
            vals: vec![],
            allow_multiple_vals: allow_multiple,
        });
    }

    fn _add_default_arguments(&mut self) {
        // Add the --help / -h argument.
        self._add_arg(CommandArg {
            short_arg: Some('h'),
            long_arg: "--help".to_owned(),
            description: "Show the help of the program".to_owned(),
            required: false,
            action: ArgAction::SpecialPrintUsage,
            vals: Vec::new(),
            allow_multiple_vals: false,
        });

        // Add the --version argument.
        self._add_arg(CommandArg {
            short_arg: None,
            long_arg: "--version".to_owned(),
            description: "Print the program version".to_owned(),
            required: false,
            action: ArgAction::SpecialPrintVersion,
            vals: Vec::new(),
            allow_multiple_vals: false,
        });
    }

    fn _add_arg(&mut self, arg: CommandArg) {
        let idx = self.args.len();

        // Add the long arg
        assert!(
            arg.long_arg.starts_with("--") && arg.long_arg.len() > 2,
            "The long argument must start with '--'"
        );
        assert!(
            !self.long_args_map.contains_key(&arg.long_arg),
            "Redefinition of CLI arg `{}`",
            arg.long_arg
        );
        self.long_args_map.insert(arg.long_arg.clone(), idx);

        // Add the sort arg.
        if let Some(short_arg) = arg.short_arg {
            assert!(
                !self.short_args_map.contains_key(&short_arg),
                "Redefinition of CLI arg `-{}`",
                short_arg
            );
            self.short_args_map.insert(short_arg, idx);
        }

        self.args.push(arg);
    }

    fn _print_usage(&self) -> ! {
        println!("{} v{}: {}", self.bin_name, self.version, self.description);
        println!("Usage: {} [args...]", self.bin_name);
        println!("Arguments:");
        for arg in &self.args {
            let has_val = match arg.action {
                ArgAction::RunWithArg(_) => true,
                ArgAction::SetOptionValue => true,
                _ => false,
            };
            let short_opt_str = match arg.short_arg {
                Some(arg) => format!(" | -{}", arg),
                None => "".to_owned(),
            };
            let val_str = if has_val { " [VAL]" } else { "" };
            println!(
                "  {}{}{}: {}",
                arg.long_arg, short_opt_str, val_str, arg.description
            );
        }
        self._exit(0);
    }

    fn _print_version(&self) -> ! {
        println!("{} v{}: {}", self.bin_name, self.version, self.description);
        self._exit(0)
    }

    // Parse all the arguments.
    // Doesn't return anything, just fill data.
    // Will exit with an error message and a return code of `1` in case of error.
    pub fn parse_args(&mut self, args: Vec<String>) {
        assert!(!self.parsed, "parse_args wwas already called before");
        self.parsed = true;

        let mut it = args.into_iter();
        while let Some(arg_str) = it.next() {
            // Look for the argument.
            let arg_idx = if arg_str.starts_with("--") {
                self.long_args_map.get(&arg_str)
            } else if arg_str.starts_with("-") && arg_str.len() == 2 {
                self.short_args_map.get(&arg_str.chars().nth(1).unwrap())
            } else {
                None
            };
            let arg_idx = match arg_idx {
                Some(idx) => *idx,
                None => {
                    eprintln!("{}: invalid argument `{}`.", self.bin_name, arg_str);
                    self._exit(1);
                }
            };
            let arg = &mut self.args[arg_idx];
            self.parsed_args.push((arg_idx, arg.vals.len()));

            // Count how many times we encountered this value.
            if !arg.allow_multiple_vals && arg.vals.len() > 0 {
                eprint!(
                    "{}: argument `{}` can only be specified one",
                    self.bin_name, arg_str
                );
                self._exit(1);
            };

            // Now apply the action.
            match &mut arg.action {
                ArgAction::RunWithoutArg(action_callback) => {
                    // We can't directly call the callback, swap it with a None value first.
                    let mut callback = None;
                    std::mem::swap(&mut *action_callback, &mut callback);
                    (callback.unwrap())();
                    arg.vals.push(None);
                }
                ArgAction::RunWithArg(action_callback) => {
                    let arg_opt = match it.next() {
                        Some(arg) => arg,
                        None => {
                            eprint!(
                                "{}: missing value for argument `{}`",
                                self.bin_name, arg_str
                            );
                            self._exit(1);
                        }
                    };
                    let mut callback = None;
                    std::mem::swap(&mut *action_callback, &mut callback);
                    (callback.unwrap())(arg_opt);
                    arg.vals.push(None);
                }
                ArgAction::SetFlag => {
                    arg.vals.push(None);
                }
                ArgAction::SetOptionValue => {
                    let val = match it.next() {
                        Some(arg) => arg,
                        None => {
                            eprint!(
                                "{}: missing value for argument `{}`",
                                self.bin_name, arg_str
                            );
                            self._exit(1);
                        }
                    };
                    arg.vals.push(Some(val));
                }
                ArgAction::SpecialPrintUsage => self._print_usage(),
                ArgAction::SpecialPrintVersion => self._print_version(),
            };
        }

        // Go through all arguments and make sure we didn't miss any.
        for arg in &self.args {
            if arg.required && arg.vals.is_empty() {
                eprint!(
                    "{}: missing required argument `{}`",
                    self.bin_name, arg.long_arg
                );
                self._exit(1);
            }
        }
    }

    pub fn get_flag(&self, long_arg: &str) -> bool {
        assert!(self.parsed, "You must call parse_args before");
        let arg = &self.args[*self.long_args_map.get(long_arg).unwrap()];
        !arg.vals.is_empty()
    }

    pub fn get_option_value(&self, long_arg: &str) -> Option<&str> {
        assert!(self.parsed, "You must call parse_args before");
        let arg = &self.args[*self.long_args_map.get(long_arg).unwrap()];
        assert!(
            !arg.allow_multiple_vals,
            "Cannot get single option if multiple val is allowed"
        );
        if arg.vals.is_empty() {
            None
        } else {
            Some(arg.vals[0].as_ref().unwrap())
        }
    }

    /// Returns the parsed arguments, in order.
    pub fn get_parsed_args<'a>(&'a self) -> impl Iterator<Item = ParsedArgument<'a>> {
        assert!(self.parsed, "Arguments must have been parsed");
        self.parsed_args.iter().map(|(arg_idx, count)| {
            let arg = &self.args[*arg_idx];
            ParsedArgument {
                long_arg: &arg.long_arg,
                val: arg.vals[*count].as_ref().map(|s| &s[..]),
            }
        })
    }

    fn _exit(&self, code: i32) -> ! {
        if code == 0 {
            exit(code)
        } else {
            // For some reason, when using exit the tests don't show anything.
            panic!("Argparse: Abort with exit code {}", code);
        }
    }
}
