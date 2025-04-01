use const_fnv1a_hash::fnv1a_hash_str_64;

use crate::{op_interfaces::BuiltinOpInterfaceWrapper, operation::OperationImpl};

// Unique identifier for the OperationType object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OperationTypeUID(u64);

impl OperationTypeUID {
    pub const fn make_from_opname(opname: &'static str) -> Self {
        Self(fnv1a_hash_str_64(opname))
    }
}

// Static information about an on operation type.
pub struct OperationTypeInfos {
    uid: OperationTypeUID,
    opname: &'static str,
    builtin_interface: Box<dyn BuiltinOpInterfaceWrapper>,
}

impl OperationTypeInfos {
    fn new(
        uid: OperationTypeUID,
        opname: &'static str,
        builtin_interface: Box<dyn BuiltinOpInterfaceWrapper>,
    ) -> Self {
        Self {
            uid,
            opname,
            builtin_interface,
        }
    }

    // Returns the unique identifier for the OperationType.
    pub fn uid(&self) -> OperationTypeUID {
        self.uid
    }

    // Returns the opname of the operation.
    pub fn opname(&self) -> &'static str {
        self.opname
    }

    // Returns the builtin op interface
    pub fn builtin_interface(&self) -> &dyn BuiltinOpInterfaceWrapper {
        &*self.builtin_interface
    }

    // TODO: Hack for parser.
    pub fn cloned_builtin_interface(&self) -> Box<dyn BuiltinOpInterfaceWrapper> {
        self.builtin_interface.clone()
    }
}

// A reference to a either a registered or unregistered type infos.
pub enum OperationTypeRef {
    Registered(OperationTypeUID),
    Unknown(String),
}

// Helper class to build an Operation type infos
pub struct OperationTypeBuilder {
    opname: Option<&'static str>,
    uid: Option<OperationTypeUID>,
    builtin_interface: Option<Box<dyn BuiltinOpInterfaceWrapper>>,
}

impl OperationTypeBuilder {
    pub fn new() -> Self {
        Self {
            opname: None,
            uid: None,
            builtin_interface: None,
        }
    }

    // Set the opname for the op.
    pub fn set_opname(&mut self, opname: &'static str) {
        assert!(self.opname.is_none());
        self.opname = Some(opname);
    }

    // Set the base implementation fort he op.
    pub fn set_impl<'a, T: OperationImpl<'a>>(&mut self) {
        assert!(self.uid.is_none());
        self.uid = Some(T::get_op_type_uid());
    }

    // Define the builtin_interface struct for the op.
    pub fn set_builtin_interface<T: BuiltinOpInterfaceWrapper + Default + 'static>(&mut self) {
        assert!(self.builtin_interface.is_none());
        let wrapper: T = Default::default();
        self.builtin_interface = Some(Box::new(wrapper));
    }

    pub fn build(self) -> OperationTypeInfos {
        let opname = self.opname.expect("Missing `opname` value");
        let uid = self.uid.expect("Missing `uid` value");
        let builtin_interface = self
            .builtin_interface
            .expect("Missing `builtin_interface` value");
        OperationTypeInfos::new(uid, opname, builtin_interface)
    }
}
