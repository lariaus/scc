use sir_core::{
    attributes::{Attribute, IntegerAttr},
    operation::GenericOperation,
    types::{FunctionType, IntegerType, Type},
};
use sir_interpreter::interpreter::SIRInterpreter;

// Which mode is used to run a program.
pub enum SIRRunnerMode {
    Interpreter,
}

pub struct SIRRunnerOpts {
    pub mode: SIRRunnerMode,
}

impl SIRRunnerOpts {
    pub fn new(mode: SIRRunnerMode) -> Self {
        Self { mode }
    }
}

enum RunnerData<'a> {
    Interpreter(SIRInterpreter<'a>),
}

impl<'a> RunnerData<'a> {
    fn make(opts: &SIRRunnerOpts, root: GenericOperation<'a>) -> Self {
        match opts.mode {
            SIRRunnerMode::Interpreter => Self::Interpreter(SIRInterpreter::new(root)),
        }
    }
}

// Helper class used to run a program.
pub struct SIRRunner<'a> {
    data: RunnerData<'a>,
}

fn parse_int_val(val: &str, ty: &IntegerType) -> Option<Attribute> {
    let val = match val.parse::<i64>() {
        Ok(val) => val,
        Err(_) => return None,
    };
    Some(IntegerAttr::new(val as u64, Type::Int(*ty)))
}

fn parse_val(val: &str, ty: &Type) -> Option<Attribute> {
    match ty {
        Type::Int(ty) => parse_int_val(val, ty),
        _ => None,
    }
}

impl<'a> SIRRunner<'a> {
    // Build a new new runner from the options and the root op.
    pub fn new(opts: SIRRunnerOpts, root: GenericOperation<'a>) -> Self {
        let data = RunnerData::make(&opts, root);
        Self { data }
    }

    // Execute a function from its input values and return the outputs.
    pub fn execute_function(
        &mut self,
        symbol_name: &str,
        inputs: Vec<Attribute>,
    ) -> Vec<Attribute> {
        match &mut self.data {
            RunnerData::Interpreter(interpeter) => {
                interpeter.interpret_function(symbol_name, inputs)
            }
        }
    }

    // Get the function type of `symbol_name`.
    // Returns none if not found.
    pub fn get_function_type(&mut self, symbol_name: &str) -> Option<&'a FunctionType> {
        match &mut self.data {
            RunnerData::Interpreter(interpeter) => interpeter.get_function_type(symbol_name),
        }
    }

    // Automatically parse the inputs for a function and return their values.
    pub fn parse_function_inputs(&mut self, symbol_name: &str, inputs: &[&str]) -> Vec<Attribute> {
        let ty = self
            .get_function_type(symbol_name)
            .expect(&format!("Function `{}` not found", symbol_name));
        let args_tys = ty.arguments();
        assert_eq!(
            inputs.len(),
            args_tys.len(),
            "Passed inputs count doesn't match real input counts"
        );
        inputs
            .iter()
            .zip(args_tys)
            .map(|(val_str, ty)| parse_val(val_str, ty).unwrap())
            .collect()
    }

    // Automatically parse the outputs for a function and return their values.
    // This is usually needed for testing purposes.
    pub fn parse_function_outputs(
        &mut self,
        symbol_name: &str,
        outputs: &[&str],
    ) -> Vec<Attribute> {
        let ty = self
            .get_function_type(symbol_name)
            .expect(&format!("Function `{}` not found", symbol_name));
        let results_tys: &[Type] = ty.results();
        assert_eq!(
            outputs.len(),
            results_tys.len(),
            "Passed output count doesn't match real output counts"
        );
        outputs
            .iter()
            .zip(results_tys)
            .map(|(val_str, ty)| parse_val(val_str, ty).unwrap())
            .collect()
    }
}
