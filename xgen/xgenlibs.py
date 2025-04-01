import os


########################################################
# XGen Definitions
########################################################


# Very basic and slow implementation of a parser
# Good enough for the small parsing things we need to do
class StringParser:

    def __init__(self, fpath, lidx, s):
        self.fpath = fpath
        self.lidx = lidx
        self.full_str = s
        self.data = s

    def is_empty(self):
        return len(self.data) == 0
    
    def startswith(self, s):
        return self.data.startswith(s)

    # "..."
    def read_string(self):
        # TODO: Properly support a string
        assert self.data[0] == '"'
        self.data = self.data[1:]
        idx = self.data.index('"')
        res = self.data[:idx]
        self.data = self.data[idx+1:].strip()
        return res

    
    # {{ ... }}
    def read_code_string(self):
        assert self.data.startswith('{{')
        self.data = self.data[2:]
        idx = self.data.index('}}')
        res = self.data[:idx]
        self.data = self.data[idx+2:].strip()
        return res.strip()
    
    # String with `##Identifier` must be resolved with the correct value
    def _string_to_value(self, s):
        if '##' in s:
            return UnresolvedString(self.fpath, self.lidx, s)
        else:
            return s
    
    # [A-Za-Z0-9_]+
    def read_identifier(self):
        idx = 0
        while idx < len(self.data) and (self.data[idx].isalnum() or self.data[idx] == '_'):
            idx += 1
        assert idx > 0
        res = self.data[:idx]
        self.data = self.data[idx:].strip()
        return res
    
    # identifier ['<' value* '>']
    def read_def_ref(self):
        # Parse the def identifier
        def_id = self.read_identifier()
        params = []
        if not self.startswith('<'):
            return UnresolvedValue(self.fpath, self.lidx,
                                   def_id, params)

        # Parse the optional parameters
        self.read_str('<')
        if not self.startswith('>'):
            while True:
                params.append(self.read_value())
                if not self.startswith(','): break
                self.read_str(',')
        self.read_str('>')

        return UnresolvedValue(self.fpath, self.lidx,
                               def_id, params)


    def read_value(self):
        if self.data.startswith('"'):
            return self._string_to_value(self.read_string())
        
        if self.data.startswith('{{'):
            return self._string_to_value(self.read_code_string())
        
        if self.startswith('true'):
            self.data = self.data[len('true'):].strip()
            return True
        
        if self.startswith('false'):
            self.data = self.data[len('false'):].strip()
            return False
        
        if len(self.data) > 0 and self.data[0].isalpha():
            return self.read_def_ref()
        
        raise Exception('{}:{}: Parsing failure for `{}`'.format(
            self.fpath, self.lidx+1, self.full_str))
    
    def read_str(self, s):
        if not self.data.startswith(s):
            raise Exception('{}:{}: Parsing failure for `{}`'.format(
                self.fpath, self.lidx+1, self.full_str))
        self.data = self.data[len(s):].strip()


    def read_eof(self):
        if len(self.data) > 0:
            raise Exception('{}:{}: Parsing failure for `{}`'.format(
                self.fpath, self.lidx+1, self.full_str))
    


# This correspond to an identifier whose value isn't bound / known yet.
# This is because we can't use a value before its defenition in XGEN.
# Also, dicts might have parameters which are represented as UnresolvedValue in their definitions
class UnresolvedValue:

    def __init__(self, fpath, lidx, id, params):
        self.fpath = fpath
        self.lidx = lidx
        self.id = id
        self.params = params

    def _get_params_count(self, def_value):
        if isinstance(def_value, RawXGenDef):
            return len(self.params)
        else:
            return 0

    def resolve(self, defs):
        # Find the def
        val_def = defs.find_def(self.id)
        if val_def is None:
            raise Exception('{}:{}: Failed to resolve unknown identifier `{}`'.format(
                self.fpath, self.lidx+1, self.id))
        
        # print('try resolve {}<{}>'.format(self.id, ','.join([str(x) for x in self.params])))
        
        # Now get the parameters
        assert len(self.params) == self._get_params_count(val_def)
        code_params = None
        if len(self.params) > 0:
            code_params = { key: resolve_value(defs, val) for (key, val) in zip(val_def.params, self.params) }
        # Switch the params
        defs_status = defs.params_defs
        defs.params_defs = code_params
        
        res = resolve_value(defs, val_def)

        # Switch back the parameters
        defs.params_defs = defs_status

        return res

# This correspond to a string with `##<Identifier>`
# This will be resolved later with the full string once we know all the definitions.
class UnresolvedString:

    def __init__(self, fpath, lidx, str):
        self.fpath = fpath
        self.lidx = lidx
        self.id = id
        self.str = str

    def resolve(self, defs):
        res = ''
        idx = 0
        while idx < len(self.str):
            if idx + 2 < len(self.str) and self.str[idx:idx+2] == '##' and self.str[idx+2].isalpha():
                def_id = ''
                idx += 2
                while idx < len(self.str) and (self.str[idx].isalpha() or self.str[idx] == '_'):
                    def_id += self.str[idx]
                    idx += 1
                assert len(def_id) > 0
                def_val = defs.find_def(def_id)
                if def_val is None:
                    raise Exception('{}:{}: Failed to resolve string `{}`: Unknown def `{}`'.format(
                        self.fpath, self.lidx+1, self.str, def_id))
                res += str(def_val)
            else:
                res += self.str[idx]
                idx += 1
        
        return res
                


# Resolve fully a value now that we have access to all defs.
# def must be a json-like object, or a UnresolvedValue / RawXGenDef
def resolve_value(defs, val):
    # Already resolved
    if val is None or isinstance(val, (str, int, float)):
        return val
    
    if isinstance(val, (RawXGenDef, UnresolvedValue, UnresolvedString)):
        return val.resolve(defs)
    
    # Resolve all items of the list
    if isinstance(val, list):
        return [resolve_value(defs, x) for x in val]
    
    # Resolve all items of the dict
    if isinstance(val, dict):
        return { key: resolve_value(defs, val) for (key, val) in val.items()}
    
    # No idea how to resolve this type.
    raise Exception('Failed to resolve value `{}`'.format(val))



# Represent a raw XGenDef structure before being resolved:
class RawXGenDef:

    def __init__(self, fpath, lidx, def_kind):
        self.fpath = fpath
        self.lidx = lidx
        self.def_kind = def_kind
        self.def_id = None
        self.params = []
        self.parent_id = None
        self.data = {}
        self.resolved_def = None

    def resolve(self, defs):
        if self.resolved_def is not None:
            return self.resolved_def
        
        
        
        # First resolve the parent if it has one
        parent_data = None
        if self.parent_id is not None:
            parent_data = self.parent_id.resolve(defs)
        
        # Then resolve the data
        data = resolve_value(defs, self.data)

        # Mix with parent_data.
        # TODO: Maybe do something special for lists ?
        if parent_data is not None:
            new_data = dict(parent_data)
            for (key, val) in data.items():
                new_data[key] = val
            data = new_data

        # If we have a def_kind resolve it
        if self.def_kind is not None:
            if self.def_kind not in _defs_kinds:
                raise Exception('{}:{}: Invalid XGENDef kind `{}`'.format(
                    self.fpath, self.lidx, self.def_kind))
            DefClass = _defs_kinds[self.def_kind]
            data = DefClass.make_from_raw_def(self, defs, data)
        
        # We can only cache the results if there is no parameters.
        if len(self.params) == 0:
            self.resolved_def = data
        return data
    
    def _set_def_line(self, defs, full_line):
        assert self.def_id is None

        # Start by reading the identifier of the def
        parser = StringParser(self.fpath, self.lidx, full_line)
        self.def_id = parser.read_identifier()
        if self.def_kind is not None:
            print('{}:{}: found Def {} for {}'.format(self.fpath, self.lidx+1, self.def_kind, self.def_id))
        if defs.find_def(self.def_id) is not None:
            raise Exception('{}:{}: Redefinition of `{}`'.format(self.fpath, self.lidx+1, self.def_id))
        defs.raw_defs[self.def_id] = self
        if parser.is_empty():
            return
        
        # Read the optional parameter names
        if parser.startswith('<'):
            parser.read_str('<')
            if not parser.startswith('>'):
                while True:
                    self.params.append(parser.read_identifier())
                    if not parser.startswith(','): break
                    parser.read_str(',')
            parser.read_str('>')

        if parser.is_empty():
            return
        
        # Read the optional direct value
        if parser.startswith('='):
            parser.read_str('=')
            self.data = parser.read_value()
            parser.read_eof()
            return

        # Read the optional parent name
        parser.read_str(':')
        self.parent_id = parser.read_def_ref()
        parser.read_eof()
       

    # Called during parsing to add a raw line to de data
    def _add_field(self, full_line):
        if not isinstance(self.data, dict):
            raise Exception('{}:{}: Cannot define fiels for defs with `=` assignment'.format(
                self.fpath, self.lidx+1))

        line = full_line
        assert line[0] == '@'
        line = line[1:]
        is_array_add = False
        if line[0] == '+':
            is_array_add = True
            line = line[1:]
        
        name, data = None, None
        if ' ' not in line:
            name = line
        else:
            name, line = [x.strip() for x in line.split(maxsplit=1)]
            parser = StringParser(self.fpath, self.lidx, line)
            data = parser.read_value()
            parser.read_eof()
        
        if is_array_add:
            if name not in self.data:
                self.data[name] = [data]
            else:
                if not isinstance(self.data[name], list):
                    raise Exception('{}: {}: {}: field `{}` isnt a list'.format(
                        self.fpath, self.lidx, full_line, name
                    ))
                self.data[name].append(data)
        else:
            if name in self.data:
                raise Exception('{}: {}: {}: redefinition of field `{}`'.format(
                        self.fpath, self.lidx, full_line, name
                ))
            self.data[name] = data


_defs_kinds = dict()

def register_def_kind(name):
    def wrapper(def_class):
        assert name not in _defs_kinds
        _defs_kinds[name] = def_class
        return def_class
    return wrapper


# Regroup all definitions found in a file
class DefsDict:

    def __init__(self):
        self.raw_defs = dict()
        self.params_defs = None
    
    # Find the definition attached to 'name'
    # Returns None if not found
    def find_def(self, name):
        if self.params_defs is not None and name in self.params_defs:
            return self.params_defs[name]
        if name in self.raw_defs:
            return self.raw_defs[name]
        return None




########################################################
# XGenDriver
########################################################


# Mark where to insert the generated code in the file
class XGenCodeInsertionPoint:

    def __init__(self, def_obj, args, lidx_beg, lidx_end):
        self.def_obj = def_obj
        self.args = args
        self.lidx_beg = lidx_beg
        self.lidx_end = lidx_end


_custom_codegens = dict()

# Register a special codegen function to handle objects with different types.
def register_custom_codegen(name):
    def wrapper(codegen_fn):
        assert name not in _custom_codegens
        _custom_codegens[name] = codegen_fn
        return codegen_fn
    return wrapper

# Analyse and find all xgen infos for one file.
class SourceFileGen:
    
    def __init__(self, defs, fpath):
        self.defs = defs
        self.fpath = fpath
        self._parse_obj = None
        self._last_def = None
        self.ips = []
        self.file_defs = []
        self.partial_parse_data = None

    # Parse the source file to read XGEN declarations.
    def parse(self):
        with open(self.fpath, 'r') as f:
            for (lidx, l) in enumerate(f.readlines()):
                self._parse_line(lidx, l)


    def _gen_code(self, ofs, ip):
        if len(ip.args) == 0:
            # Default def_obj codegen
            return ip.def_obj.resolve(self.defs).generate_code(ofs)
        

        codegen_name = ip.args[0]
        args = ip.args[1:]
        if codegen_name not in _custom_codegens:
            raise Exception('Unknown gen kind `{}`'.format(codegen_name))
        codegen_fun = _custom_codegens[codegen_name]

        return codegen_fun(ofs, self.defs, self.file_defs, args)

    # Generate the source code for all defs
    def update(self):

        # Build a map: lidx => ip
        ips_map = dict()
        for ip in self.ips:
            assert ip.lidx_beg not in ips_map
            ips_map[ip.lidx_beg] = ip

        # First write the content to a completely new file.
        tmp_path = '/tmp/xgen.data'
        current_ip = None
        with open(tmp_path, 'w') as ofs:
            with open(self.fpath, 'r') as f:
                for (lidx, l) in enumerate(f.readlines()):

                    if current_ip is None and lidx in ips_map:
                        current_ip = ips_map[lidx]
                    
                    if current_ip is None:
                        ofs.write(l)
                    elif current_ip.lidx_end == lidx:
                        self._gen_code(ofs, current_ip)
                        current_ip = None

        # Then replace the file
        os.rename(tmp_path, self.fpath)




    def _parse_line(self, lidx, l):
        if self._parse_obj is None:
            if l.startswith('// @XGENDEF'):
                self._parse_def_line(lidx, l)

            elif l.startswith('// @XGENBEGIN'):
                self._parse_code_insertion_point(lidx, l)

        elif isinstance(self._parse_obj, RawXGenDef):
            self._parse_def_line(lidx, l)
        elif isinstance(self._parse_obj, XGenCodeInsertionPoint):
            self._parse_code_insertion_point(lidx, l)
        else:
            raise Exception('Invalid state')

    def _parse_def_line(self, lidx, l):
        l = l.strip()

        def finish_line():
            if self.partial_parse_data is not None:
                if self._parse_obj.def_id is None:
                    self._parse_obj._set_def_line(self.defs, self.partial_parse_data)
                else:
                    self._parse_obj._add_field(self.partial_parse_data)
                self.partial_parse_data = None


        if self._parse_obj is None:
            # Initialize the definition.
            
            l = l[len('// @XGENDEF'):]
            def_kind = None
            if l.startswith(':'):
                # // @XGENDEF:<DefKind> <DefIdentifier>
                # Custom Def object Kind
                l = l[1:]
                def_kind, l = [x.strip() for x in l.split(maxsplit=1)]
            
            self.partial_parse_data = l.strip()

            self._parse_obj = RawXGenDef(self.fpath, lidx, def_kind)
            self.file_defs.append(self._parse_obj)
            return
        
        if not l.startswith('//'):
            # We're done parsing the def.
            finish_line()
            self._last_def = self._parse_obj
            self._parse_obj = None
            return
        
        l = l[2:].strip()

        if l.startswith('#'):
             # Ignore comment lines
            return
        
        if l.startswith('@'):
            # We're starting a new data line
            finish_line()
            self.partial_parse_data = l
            return


        # This is the next part of the current data line.
        if self.partial_parse_data is None:
            self.partial_parse_data = l
        else:
            self.partial_parse_data += ' ' + l
        

    def _parse_code_insertion_point(self, lidx, l):
        if self._parse_obj is None:
            # Start the parsing

            l = l[len('// @XGENBEGIN'):].strip()
            args = l.split()

            
            if len(args) == 0 and self._last_def is None:
                raise Exception('No definition found yet')
            self._parse_obj = XGenCodeInsertionPoint(self._last_def, args, lidx+1, lidx+1)
            self.ips.append(self._parse_obj)
            self._last_def = None
        
        elif l.startswith('// @XGENEND'):
            # Reached the end
            self._parse_obj.lidx_end = lidx-1
            self._parse_obj = None

# Class used to run the XGEN Script on a specific directory
class XGENDriver:

    def __init__(self, root_dir):
        self.root_dir = root_dir
        self.src_exts = ['.rs']
        self.defs = DefsDict()

    def run(self):
        to_update = []
        # First go and parse all files
        for root, fdirs, fnames in os.walk(self.root_dir):
            for fname in fnames:
                if (any([fname.endswith(ext) for ext in self.src_exts])):
                    gen = SourceFileGen(self.defs, os.path.join(root, fname))
                    gen.parse()
                    if len(gen.ips) > 0:
                        to_update.append(gen)
        
        # Then update all files that needs to be modified
        for gen in to_update:
            gen.update()