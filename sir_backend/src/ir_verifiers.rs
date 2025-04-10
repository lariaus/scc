pub mod ir_backend_checks {
    use sir_core::{
        attributes::{ArrayAttr, Attribute, AttributeSubClass, IntegerAttr, RegisterAttr},
        ir_context::IRContext,
        types::IntegerType,
    };

    pub fn pred_is_valid_register_attr(_ctx: &IRContext, attr: &Attribute) -> bool {
        // Make sure have a register attr.
        let _attr = match attr.cast::<RegisterAttr>() {
            Some(attr) => attr,
            None => return false,
        };

        // @[TODO][I11][SIR-BACKEND]: Re-enable verifiers for register attrs.
        // // Make sure the register exists.
        // let backend = SIRBackend::get(ctx);
        // let reg_infos = match backend.backend_desc().get_registers_desc().get_reg_desc(attr.name()) {
        //     Some(infos) => infos,
        //     None => return false,
        // };

        // // Check if the usage match.
        // if reg_infos.is_virtual() != attr.is_virtual() {
        //     return false;
        // }

        true
    }

    pub fn pred_is_valid_register_attr_or_imm(ctx: &IRContext, attr: &Attribute) -> bool {
        if let Some(int_attr) = attr.cast::<IntegerAttr>() {
            let int_type = match int_attr.get_type().unwrap().cast::<IntegerType>() {
                Some(ty) => ty,
                None => return false,
            };
            return int_type.is_index_type();
        }

        pred_is_valid_register_attr(ctx, attr)
    }

    pub fn pred_is_valid_registers_list_attr(ctx: &IRContext, attr: &Attribute) -> bool {
        let attr = match attr.cast::<ArrayAttr>() {
            Some(attr) => attr,
            None => return false,
        };
        attr.elements()
            .iter()
            .all(|reg| pred_is_valid_register_attr(ctx, reg))
    }
}

// Define an attr that must be a Register
// @XGENDEF RegisterAttr<Name> : AttrBase<Name, "&'a RegisterAttr",
//    {{ self.get_attr($attr_symbol).expect("Missing `##Name` attribute").cast::<RegisterAttr>().expect("`##Name` attribute must be a register attribute") }},
//    {{ ir_checks::verif_has_attr_as(diagnostics, self.generic(), $attr_symbol, |attr| ir_backend_checks::pred_is_valid_register_attr(self.get_context(), attr), "register")?; }}
// >

// Define an attr that must be a Register or an immediate (index integer)
// @XGENDEF RegisterOrImmAttr<Name> : AttrBase<Name, "&'a Attribute",
//    {{ self.get_attr($attr_symbol).expect("Missing `##Name` attribute") }},
//    {{ ir_checks::verif_has_attr_as(diagnostics, self.generic(), $attr_symbol, |attr| ir_backend_checks::pred_is_valid_register_attr_or_imm(self.get_context(), attr), "register or immediate")?; }}
// >

// Define an attr that must be a list of registers
// @XGENDEF RegisterListAttr<Name> : AttrBase<Name, "&'a Attribute",
//    {{ self.get_attr($attr_symbol).expect("Missing `##Name` attribute") }},
//    {{ ir_checks::verif_has_attr_as(diagnostics, self.generic(), $attr_symbol, |attr| ir_backend_checks::pred_is_valid_registers_list_attr(self.get_context(), attr), "registers list")?; }}
// >

// Add more defs ...
