from enum import Enum
import re

from xgenlibs import register_def_kind, register_custom_codegen

# XGEN Definitions specific to the SIR library


########################################################
# Utils functions
########################################################

# Convert a string for CamelCase to snake_case.
def camel_to_snake_case(s):
    return re.sub(r'(?<!^)(?=[A-Z])', '_', s).lower()

# Rewrite code like `foo(x, $mynum)`, values_dict={"mynum": 42} with the right values
def rewrite_code_with_repl(s, values_dict):
    # TODO: optimize the implementation
    for (key, val) in values_dict.items():
        s = s.replace('$' + key, str(val))
    return s

########################################################
# XGEN SIROp Definition
########################################################

# Helper method to get rid of all decorators around a parsed object
def unwrap_decorators(raw_data):
    decorators = []
    while True:
        if not isinstance(raw_data, dict) or '__decorator_kind' not in raw_data:
            break
        decorators.append(raw_data['__decorator_kind'])
        raw_data = raw_data['value']
    return raw_data, decorators



# Wrapper class around a ModBase dict.
class ModDefInfos:

    def __init__(self, raw_data):
        self.kind = raw_data['kind']
        self.data = raw_data['data']

# Wrapper class around a ValueBase dict.
class ValueDefInfos:

    def __init__(self, raw_data):
        self.name = raw_data["name"]
        self.is_variadic = raw_data["is_variadic"]
        self.verif_code = raw_data["verif_code"]

# Wrapper class around an AttrBase dict.
class AttrDefInfos:

    def __init__(self, raw_data):
        raw_data, decorators = unwrap_decorators(raw_data)
        self.name = raw_data["name"]
        self.attr_type = raw_data["attr_type"]
        self.getter_code = raw_data["getter_code"]
        self.verif_code = raw_data["verif_code"]
        self.optional = 'optional' in decorators


# Represent the definition of an SIR Operation class.
# Will automatically generate all the boilerplate code for it.
@register_def_kind("SIROp")
class OpDefInfos:

    def __init__(self, defs, class_name, raw_data):
        self.defs = defs
        self.class_name = class_name
        self.opname = None
        self.inputs = []
        self.outputs = []
        self.attrs = []
        self.blocks = []
        self.tags = []
        self.interfaces = []
        self.mods = []
        self.custom_print_parse = False
        self.has_constant_builder = False
        self.has_custom_verifier = False
        self.disable_default_builder = False
        self.extra_verif_lines = []

        # Go through the raw data to initialize the object
        self.opname = raw_data["opname"]

        if "block" in raw_data:
            self.blocks = raw_data["block"]
        
        if "attr" in raw_data:
            self.attrs = [AttrDefInfos(x) for x in raw_data["attr"]]
        
        if "input" in raw_data:
            self.inputs = [ValueDefInfos(x) for x in raw_data["input"]]
        
        if "output" in raw_data:
            self.outputs = [ValueDefInfos(x) for x in raw_data["output"]]

        if "mod" in raw_data:
            self.mods = [ModDefInfos(x) for x in raw_data["mod"]]

        if "tags" in raw_data:
            self.tags = raw_data["tags"]

        builtin_interface = self.defs.find_def("BuiltinOp").resolve(self.defs)
        assert isinstance(builtin_interface, InterfaceDefInfos)
        self.interfaces.append(builtin_interface)
        if "interfaces" in raw_data:
            self.interfaces += raw_data["interfaces"]
            for obj in self.interfaces:
                assert isinstance(obj, InterfaceDefInfos)

        self.custom_print_parse = 'custom_print_parse' in raw_data
        self.has_custom_verifier = 'verifier' in raw_data
        self.has_constant_builder = 'constant_builder' in raw_data
        self.disable_default_builder = 'disable_default_builder' in raw_data

        name_snake_case = camel_to_snake_case(self.class_name)
        if name_snake_case.endswith('_op'):
            name_snake_case = name_snake_case[:-3]
        self.name_snake_case = name_snake_case
        self.class_name_upper = name_snake_case.upper()


    @staticmethod
    def make_from_raw_def(raw_def, defs, raw_data):
        return OpDefInfos(defs, raw_def.def_id, raw_data)
    

    def has_variadic_inputs(self):
        return any([v.is_variadic for v in self.inputs])
    
    def has_variadic_outputs(self):
        return any([v.is_variadic for v in self.outputs])

    def generate_code(self, ofs):
        # Prepare data for generation
        if self.opname is None:
            raise Exception('Missing opname for {}'.format(self.class_name))

        # Start by applying all mods
        for mod in self.mods:
            self._apply_mod(mod)

        # Generate the base code
        ofs.write(CODE_OPDEF_BASE.format(
            class_name=self.class_name,
            class_name_upper=self.class_name_upper,
            opname=self.opname
        ))


        # Generate extra names for the attrs
        if len(self.attrs) > 0:
            for attr in self.attrs:
                ofs.write('const {class_name_upper}_ATTR_{attr_name_upper}: &\'static str = "{attr_name}";\n'.format(
                    class_name_upper=self.class_name_upper,
                    attr_name = attr.name,
                    attr_name_upper = attr.name.upper()
                ))
            ofs.write('\n')
                

        # Generate extra functions
        ofs.write("impl<'a> {}<'a> {{\n".format(self.class_name))

        # Generate all inputs getters
        for (idx, val) in enumerate(self.inputs):
            # TODO: Support properly variadics
            if val.is_variadic:
                break
            ofs.write(CODE_OPDEF_GETTER_INPUT.format(
                name=val.name,
                idx=idx,
            ))
        
        # Generate all outputs getters
        for (idx, val) in enumerate(self.outputs):
            # TODO: Support properly variadics
            if val.is_variadic:
                break
            ofs.write(CODE_OPDEF_GETTER_OUTPUT.format(
                name=val.name,
                idx=idx,
            ))

        # Generate all attributes getters
        for attr in self.attrs:
            full_sym_name = '{}_ATTR_{}'.format(self.class_name_upper, attr.name.upper())
            attr_ty = attr.attr_type
            attr_getter_code = attr.getter_code
            if attr.optional:
                # For optional attrs we need to add extra code to check for missing attr.
                attr_ty = 'Option<{}>'.format(attr_ty)
                attr_getter_code = 'if self.has_attr($attr_symbol) {{ Some({}) }} else {{ None }}'.format(attr_getter_code)
            
            ofs.write('    pub fn get_{}(&self) -> {} {{\n'.format(attr.name, attr_ty))
            ofs.write('        {}\n'.format(rewrite_code_with_repl(attr_getter_code, {
                'attr_symbol': full_sym_name,
            })))
            ofs.write('    }\n\n')
            if attr.optional:
                ofs.write(COPE_OPDEF_GETTER_OPTIONAL_ATTR.format(name=attr.name, attr_sym_name=full_sym_name))
            else:
                ofs.write(COPE_OPDEF_GETTER_ATTR.format(name=attr.name, attr_sym_name=full_sym_name))
        
        # Generate all blocks getters
        for (idx, block_name) in enumerate(self.blocks):
            ofs.write(CODE_OPDEF_GETTER_BLOCK.format(
                name=block_name,
                idx=idx,
            ))

        ofs.write('}\n\n')

        # Generate interfaces data
        for interface in self.interfaces:
            self._gen_interface_impl(ofs, interface)

        # Generate some extra functions.
        ofs.write("impl<'a> {}<'a> {{\n".format(self.class_name))
        if not self.disable_default_builder:
            self._gen_default_builder(ofs)
        self._gen_verify_fun(ofs)
        self._gen_builtin_clone_fun(ofs)
        if not self.custom_print_parse:
            self._gen_custom_print_fun(ofs)
            self._gen_custom_parse_fun(ofs)
        ofs.write('}\n\n')

        # Generate the register for the op.
        self._gen_register_fun(ofs)
        ofs.write('\n\n')

    def _gen_register_fun(self, ofs):
        ofs.write('fn register_{}_op(ctx: &mut ContextRegistry) {{'.format(self.name_snake_case))
        ofs.write('    let mut infos = OperationTypeBuilder::new();\n')
        ofs.write('    infos.set_opname({}_OPNAME);\n'.format(self.class_name_upper))
        ofs.write('    infos.set_impl::<{}>();\n'.format(self.class_name))
        ofs.write('    infos.set_builtin_interface::<{}BuiltinOpInterfaceImpl>();\n'.format(self.class_name))
        if len(self.tags) > 0:
            ofs.write('    infos.set_tags(&[{}]);\n'.format(
                ', '.join(self.tags)
            ))
        for interface in self.interfaces:
            ofs.write('    infos.add_interface::<{}{}InterfaceImpl>();\n'.format(
                self.class_name, interface.class_name))
        ofs.write('    ctx.register_operation(infos.build());\n')
        ofs.write('}\n\n')


    def _gen_default_builder(self, ofs):

        # Build the signature
        ofs.write("    pub fn build(")
        first_input = True
        for val in self.inputs:
            if not first_input: ofs.write(', ')
            first_input = False
            ofs.write('{}: ValueID'.format(val.name))
        for val in self.attrs:
            if not first_input: ofs.write(', ')
            first_input = False
            ofs.write('{}: Attribute'.format(val.name))
        for val in self.blocks:
            if not first_input: ofs.write(', ')
            first_input = False
            ofs.write('{}: BlockID'.format(val.name))
        for val in self.outputs:
            if not first_input: ofs.write(', ')
            first_input = False
            ofs.write('{}: Type'.format(val.name))
        ofs.write(') -> OpImplBuilderState<Self> {\n')

        if first_input:
            # Special case, nohting to build, don't make it a mut.
            ofs.write("        let st = OpImplBuilderState::make();\n")
        else:
            ofs.write("        let mut st = OpImplBuilderState::make();\n")
        
        # Set the inputs.
        if len(self.inputs) > 0:
            ofs.write("        st.set_inputs(vec![{}]);\n".format(
                ', '.join([x.name for x in self.inputs])))

        # Set the attrs.
        for attr in self.attrs:
            attr_id_name = '{}_ATTR_{}'.format(self.class_name_upper, attr.name.upper())
            ofs.write("        st.set_attr({}, {});\n".format(
                attr_id_name, attr.name))

        # Set the blocks.
        if len(self.blocks) > 0:
            ofs.write("        st.set_blocks(vec![{}]);\n".format(
                ', '.join([x.name for x in self.blocks])))

        # Set the outputs types.
        if len(self.outputs) > 0:
            ofs.write("        st.set_outputs_types(vec![{}]);\n".format(
                ', '.join([x.name for x in self.outputs])))
        
        # Return the builder.
        ofs.write("        st\n")
        ofs.write("    }\n")


        #pub fn build(lhs: ValueID, rhs: ValueID, out: Type) -> OpImplBuilderState<Self> {
        #    let mut st = OpImplBuilderState::make();
        #    st.set_inputs(vec![lhs.into(), rhs.into()]);
        #    st.set_outputs_types(vec![out]);
        #    st
        #}

    def _gen_verify_fun(self, ofs):
        ofs.write("    pub fn verify(&self, diagnostics: &mut DiagnosticsEmitter) -> ErrorOrSuccess {\n")
        
        # Check the inputs.
        if not self.has_variadic_inputs():
            ofs.write("        ir_checks::verif_inputs_count(diagnostics, self.generic(), {})?;\n".format(len(self.inputs)))
            for (idx, val) in enumerate(self.inputs):
                if len(val.verif_code) != 0:
                    ofs.write("        {}\n".format(rewrite_code_with_repl(val.verif_code, {
                        'is_input': 'true',
                        'idx': idx,
                        'value_name': '"{}"'.format(val.name)
                    })))

        # Check the attributes.
        for attr in self.attrs:
            full_sym_name = '{}_ATTR_{}'.format(self.class_name_upper, attr.name.upper())
            ofs.write("        {}\n".format(rewrite_code_with_repl(attr.verif_code, {
                'attr_symbol': full_sym_name,
            })))


        # Check the outputs.
        if not self.has_variadic_outputs():
            ofs.write("        ir_checks::verif_outputs_count(diagnostics, self.generic(), {})?;\n".format(len(self.outputs)))
            for (idx, val) in enumerate(self.outputs):
                if len(val.verif_code) != 0:
                    ofs.write("        {}\n".format(rewrite_code_with_repl(val.verif_code, {
                        'is_input': 'false',
                        'idx': idx,
                        'value_name': '"{}"'.format(val.name)
                    })))

        # Check the blocks.
        ofs.write("        ir_checks::verif_blocks_count(diagnostics, self.generic(), {})?;\n".format(len(self.blocks)))

        # Call extra verif code.
        for line in self.extra_verif_lines:
            ofs.write("        {}\n".format(line))

        # Call the custom verifier if needed
        if self.has_custom_verifier:
            ofs.write("        self.verify_op(diagnostics)?;\n")

        ofs.write("        Ok(())\n")
        ofs.write("    }\n")

    def _gen_builtin_clone_fun(self, ofs):
        ofs.write("    pub fn clone() -> BuiltinOpInterfaceImplWrapper {\n")
        ofs.write("        BuiltinOpInterfaceImplWrapper::new(Box::new({}BuiltinOpInterfaceImpl))\n".format(self.class_name))
        ofs.write("    }\n")

    def _gen_custom_print_fun(self, ofs):
        ofs.write(CODE_OPDEF_DEFAULT_CUSTOM_PRINT_FUN)

    def _gen_custom_parse_fun(self, ofs):
        ofs.write(CODE_OPDEF_DEFAULT_CUSTOM_PARSE_FUN)

    def _apply_mod(self, mod):
        if mod.kind == 'op-verifier-mod':
            self.extra_verif_lines.append(mod.data)
        else:
            raise Exception('Unknown mod kind `{}`'.format(mod.kind))

    def _gen_interface_impl(self, ofs, interface):
        ofs.write('// Wrapper struct for the {} interface implementation.\n'.format(interface.class_name))
        ofs.write('#[derive(Default)]\n')
        ofs.write('pub struct {}{}InterfaceImpl;\n\n'.format(self.class_name, interface.class_name))
        ofs.write('impl {}InterfaceImpl for {}{}InterfaceImpl {{\n'.format(interface.class_name, self.class_name, interface.class_name))

        for method in interface.methods:
            method = method.clone()

            # Add context / data params for methods only
            if method.fun_kind == FunctionKind.Method:
                method.parameters.append("'a")
                method.insert_input(0, "ctx", "&'a IRContext")
                method.insert_input(1, "data", "&'a OperationData")

            # Always add a self argument.
            method.insert_input(0, "&self", "")

            ofs.write('    {} {{\n'.format(method.to_fn_signature()))
            if method.fun_kind == FunctionKind.Method:
                ofs.write('        GenericOperation::make_from_data(ctx, data).cast::<{}>().unwrap().{}({})\n'.format(
                    self.class_name, method.fun_name, 
                    ', '.join(method.inputs_names[3:])))
            else:
                # static method
                ofs.write('        {}::{}({})\n'.format(
                    self.class_name, method.fun_name, 
                    ', '.join(method.inputs_names[1:])))
            ofs.write('    }\n')

        ofs.write('}\n\n')

        # Add the builder implementation
        ofs.write(CODE_OPDEF_INTERFACEIMPL_BUILDER.format(
            op_name = self.class_name,
            interface_name = interface.class_name
        ))





########################################################
# XGEN SIRInterface Definition
########################################################



class FunctionKind(Enum):
    Function = 0
    Method = 1
    StaticMethod = 2

# Represent a function / method with all I/O / parameters.
class FunctionSignature:

    def __init__(self, fun_kind, fun_name, inputs_names, inputs_types, output_type, parameters):
        assert len(inputs_names) == len(inputs_types)

        self.fun_kind = fun_kind
        self.fun_name = fun_name
        self.inputs_names = inputs_names
        self.inputs_types = inputs_types
        self.output_type = output_type
        self.parameters = parameters

    def clone(self):
        return FunctionSignature(self.fun_kind, self.fun_name, list(self.inputs_names), list(self.inputs_types), self.output_type, list(self.parameters))

    def insert_input(self, idx, name, ty):
        self.inputs_names.insert(idx, name)
        self.inputs_types.insert(idx, ty)


    def to_fn_signature(self):
        # Name
        res = 'fn {}'.format(self.fun_name)

        # Parameters
        if len(self.parameters) > 0:
            res += '<{}>'.format(', '.join(self.parameters))
        
        def get_input(idx):
            name, ty = self.inputs_names[idx], self.inputs_types[idx]
            if len(ty) == 0:
                return name
            return '{}: {}'.format(name, ty)    

        # Inputs
        res += '({})'.format(', '.join([
            get_input(idx) for idx in range(len(self.inputs_names))
        ]))

        # Output
        if self.output_type is not None:
            res += ' -> {}'.format(self.output_type)

        return res

    @staticmethod
    def from_signature_string(fun_kind, l):
        inputs_names = []
        inputs_types = []
        parameters = []
        output_type = None

        # Parse the opname
        pos = l.index('(')
        fun_name = l[:pos].strip()
        l = l[pos+1:].strip()

        # Parse the arguments
        while l[0] != ')':
            pos = l.index(':')
            arg_name = l[:pos].strip()
            l = l[pos+1:].strip()
            assert l[0] == '"'
            l = l[1:]
            pos = l.index('"')
            arg_type = l[:pos].strip()
            l = l[pos+1:].strip()
            if l[0] == ',':
                l = l[1:].strip()
            
            inputs_names.append(arg_name)
            inputs_types.append(arg_type)
        l = l[1:].strip()

        # Parse optional output type
        if len(l) > 0:
            assert l[:2] == '->'
            l = l[2:].strip()
            assert l[0] == '"'
            l = l[1:]
            pos = l.index('"')
            output_type = l[:pos].strip()
            l = l[pos+1:].strip()
        assert len(l) == 0

        return FunctionSignature(fun_kind, fun_name, inputs_names, inputs_types, output_type, parameters)

@register_def_kind("SIRInterface")
class InterfaceDefInfos:

    def __init__(self, defs, class_name, raw_data):
        self.defs = defs
        self.class_name = class_name
        self.methods = []

        # Go through the raw data
        if "method" in raw_data:
            for method_signature in raw_data["method"]:
                 self.methods.append(FunctionSignature.from_signature_string(FunctionKind.Method, method_signature))

        if "staticmethod" in raw_data:
            for method_signature in raw_data["staticmethod"]:
                 self.methods.append(FunctionSignature.from_signature_string(FunctionKind.StaticMethod, method_signature))

    @staticmethod
    def make_from_raw_def(raw_def, defs, raw_data):
        return InterfaceDefInfos(defs, raw_def.def_id, raw_data)

    def generate_code(self, ofs):
        class_name_snake_case = camel_to_snake_case(self.class_name)
        class_name_upper = class_name_snake_case.upper()

        # First make the interface uid.
        ofs.write('// Unique identifier for the {} Interface\n'.format(self.class_name))
        ofs.write('const {}_INTERFACE_UID: OpInterfaceUID = OpInterfaceUID::make_from_interface_identifier("{}");\n\n'.format(
            class_name_upper, self.class_name))
        
        # Define the interface implementation trait.
        ofs.write('// Interface Implementation for the {} interface.\n'.format(self.class_name))
        ofs.write('pub trait {}InterfaceImpl {{\n'.format(self.class_name))

        for method in self.methods:
            method = method.clone()

            # Add context / data params for methods only
            if method.fun_kind == FunctionKind.Method:
                method.parameters.append("'a")
                method.insert_input(0, "ctx", "&'a IRContext")
                method.insert_input(1, "data", "&'a OperationData")

            # Always add a self argument.
            method.insert_input(0, "&self", "")
            ofs.write('    {};\n'.format(method.to_fn_signature()))

        ofs.write('}\n\n')

        # Define the interface wrapper class
        ofs.write(CODE_INTERFACEDEF_WRAPPER.format(
            class_name=self.class_name
        ))

        # Define the helper functions for the static methods
        ofs.write('//Definition of helper functions for the static methods of {} interface.\n'.format(self.class_name))
        ofs.write('impl {}InterfaceImplWrapper {{\n'.format(self.class_name))
        for method in self.methods:
            if method.fun_kind != FunctionKind.StaticMethod:
                continue

            method = method.clone()
            # Add a self argument.
            method.insert_input(0, "&self", "")

            ofs.write('    pub {} {{\n'.format(method.to_fn_signature()))
            ofs.write('        self.dyn_impl.{}({})\n'.format(method.fun_name,
                ', '.join(method.inputs_names[1:])))
            ofs.write('    }\n')
        ofs.write('}\n\n')

        # Define the Operation object for the interface.
        ofs.write(CODE_INTERFACEDEF_OP_IMPL.format(
            class_name=self.class_name,
            class_name_upper=class_name_upper
        ))

        # Define the methods for the Op Interface
        ofs.write('// Methods implementation for {}.\n'.format(self.class_name))
        ofs.write("impl<'a> {}<'a> {{\n".format(self.class_name))
        for method in self.methods:
            if method.fun_kind != FunctionKind.Method:
                continue

            method = method.clone()
            # Add a self argument.
            method.insert_input(0, "&self", "")

            ofs.write('    pub {} {{\n'.format(method.to_fn_signature()))
            ofs.write('        self.wrapper.dyn_impl.{}({})\n'.format(method.fun_name,
                ', '.join(['&self.ctx', '&self.data'] + method.inputs_names[1:])))
            ofs.write('    }\n')
        ofs.write('}\n\n')


########################################################
# Codegenfunction RegisterOps
########################################################


@register_custom_codegen("RegisterOps")
def generate_registered_ops(ofs, defs, file_defs, args):
    fun_name, = args
    
    ofs.write('pub fn {}(ctx: &mut IRContext) {{\n'.format(fun_name))

    # Build the registrer
    ofs.write('    ContextRegistry::exec_register_fn(ctx, "__sir/ops/{}", |mut registry| {{\n'.format(fun_name))

    # Go through all op defs
    for xgen_def in file_defs:
        if xgen_def.def_kind != 'SIROp':
            continue
        op_def = xgen_def.resolve(defs)
        ofs.write('        register_{}_op(&mut registry);\n'.format(op_def.name_snake_case))
        if op_def.has_constant_builder:
            ofs.write('        registry.register_constant_builder({}::build_constant);\n'.format(op_def.class_name))

    ofs.write('    });\n')
    ofs.write('}\n\n')



########################################################
# Raw Generated Code Strings
########################################################

# Strings used to generate code

CODE_OPDEF_BASE = '''
// Code automatically generated by sir_core/scripts/xgen.py

const {class_name_upper}_OPNAME: &'static str = "{opname}";
const {class_name_upper}_TYPE_UID: OperationTypeUID = OperationTypeUID::make_from_opname({class_name_upper}_OPNAME);

pub struct {class_name}<'a> {{
    ctx: &'a IRContext,
    data: &'a OperationData,
}}

impl<'a> OperationImpl<'a> for {class_name}<'a> {{
    fn make_from_data(ctx: &'a IRContext, data: &'a OperationData) -> Self {{
        Self {{ ctx, data }}
    }}

    fn get_op_data(&self) -> &'a OperationData {{
        self.data
    }}

    fn get_context(&self) -> &'a IRContext {{
        self.ctx
    }}

    fn get_op_type_uid() -> OperationTypeUID {{
        {class_name_upper}_TYPE_UID
    }}
}}

'''

COPE_OPDEF_GETTER_ATTR = '''
    pub fn get_{name}_attr(&self) -> &'a Attribute {{
        self.get_attr({attr_sym_name}).expect("Missing `{name}` attribute")
    }}

'''

COPE_OPDEF_GETTER_OPTIONAL_ATTR = '''
    pub fn get_{name}_attr(&self) -> Option<&'a Attribute> {{
        self.get_attr({attr_sym_name})
    }}

'''

CODE_OPDEF_GETTER_BLOCK = '''
    pub fn get_{name}(&self) -> Block<'a> {{
        self.get_block({idx})
    }}

'''

CODE_OPDEF_GETTER_INPUT = '''
    pub fn get_{name}(&self) -> Value<'a> {{
        self.get_input({idx})
    }}

'''

CODE_OPDEF_GETTER_OUTPUT = '''
    pub fn get_{name}(&self) -> Value<'a> {{
        self.get_output({idx})
    }}

'''

CODE_INTERFACEDEF_WRAPPER = '''
// Wrapper object holding an implementation of the {class_name} interface.
pub struct {class_name}InterfaceImplWrapper {{
    dyn_impl: Box<dyn {class_name}InterfaceImpl>,
}}

impl {class_name}InterfaceImplWrapper {{
    pub fn new(dyn_impl: Box<dyn {class_name}InterfaceImpl>) -> Self {{
        Self {{ dyn_impl }}
    }}
}}

impl OpInterfaceWrapper for {class_name}InterfaceImplWrapper {{
    fn as_any(&self) -> &dyn Any {{
        self
    }}
}}

'''

CODE_INTERFACEDEF_OP_IMPL = '''
// Object to materialize an operation that implements the {class_name} interface.
pub struct {class_name}<'a> {{
    wrapper: &'a {class_name}InterfaceImplWrapper,
    ctx: &'a IRContext,
    data: &'a OperationData,
}}

impl<'a> OpInterfaceObject<'a> for {class_name}<'a> {{
    fn get_interface_uid() -> OpInterfaceUID {{
        {class_name_upper}_INTERFACE_UID
    }}

    fn make(
        wrapper: &'a dyn OpInterfaceWrapper,
        ctx: &'a IRContext,
        data: &'a OperationData,
    ) -> Self {{
        let wrapper = wrapper
            .as_any()
            .downcast_ref::<{class_name}InterfaceImplWrapper>()
            .unwrap();
        Self {{ ctx, data, wrapper }}
    }}
}}

impl<'a> OperationImpl<'a> for {class_name}<'a> {{
    fn make_from_data(_ctx: &'a IRContext, _data: &'a OperationData) -> Self {{
        panic!("Not supported for InterfaceOp")
    }}

    fn get_op_data(&self) -> &'a OperationData {{
        &self.data
    }}

    fn get_context(&self) -> &'a IRContext {{
        &self.ctx
    }}

    fn get_op_type_uid() -> OperationTypeUID {{
        panic!("Cannot get the TypeUID of an InterfaceOp")
    }}
}}

'''

CODE_OPDEF_INTERFACEIMPL_BUILDER = '''
// Interface builder for {interface_name} interface.
impl OpInterfaceBuilder for {op_name}{interface_name}InterfaceImpl {{
    type InterfaceObjectType = {interface_name}<'static>;

    fn build_interface_object() -> Box<dyn OpInterfaceWrapper + 'static> {{
        let wrapper = {interface_name}InterfaceImplWrapper::new(Box::new(Self));
        Box::new(wrapper)
    }}
}}

'''

CODE_OPDEF_DEFAULT_CUSTOM_PRINT_FUN = '''
    pub fn custom_print(&self, printer: &mut IRPrinter) -> Result<(), std::io::Error> {
        printer.print_op_generic_form_impl(self.generic())
    }

'''

CODE_OPDEF_DEFAULT_CUSTOM_PARSE_FUN = '''
    pub fn custom_parse(parser: &mut IRParser, _ctx: &mut IRContext, _st: &mut OperationParserState) -> Option<()> {
        let loc = parser.get_last_token_loc();
        emit_error(parser, &loc, format!("custom form representation not supported"));
        None
    }

'''