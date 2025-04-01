use std::collections::{HashMap, HashSet};

use const_fnv1a_hash::fnv1a_hash_str_64;

use crate::{
    op_interfaces::{
        BuiltinOpInterfaceImpl, BuiltinOpInterfaceImplWrapper, OpInterfaceBuilder, OpInterfaceUID,
        OpInterfaceWrapper,
    },
    op_tags::OperationTag,
    operation::OperationImpl,
};

use crate::op_interfaces::OpInterfaceObject;

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
    builtin_interface: BuiltinOpInterfaceImplWrapper,
    tags: HashSet<OperationTag>,
    interfaces: HashMap<OpInterfaceUID, Box<dyn OpInterfaceWrapper>>,
}

impl OperationTypeInfos {
    fn new(
        uid: OperationTypeUID,
        opname: &'static str,
        builtin_interface: BuiltinOpInterfaceImplWrapper,
        tags: HashSet<OperationTag>,
        interfaces: HashMap<OpInterfaceUID, Box<dyn OpInterfaceWrapper>>,
    ) -> Self {
        Self {
            uid,
            opname,
            builtin_interface,
            tags,
            interfaces,
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
    pub fn builtin_interface(&self) -> &BuiltinOpInterfaceImplWrapper {
        &self.builtin_interface
    }

    // Returns true if the op has the associated tag.
    pub fn has_tag(&self, tag: OperationTag) -> bool {
        self.tags.contains(&tag)
    }

    // TODO: Hack for parser.
    pub fn cloned_builtin_interface(&self) -> BuiltinOpInterfaceImplWrapper {
        self.builtin_interface.clone()
    }

    // Find the interface implementation for `uid`.
    // Returns None if there is no implementation of this interface.
    pub fn get_interface(&self, uid: OpInterfaceUID) -> Option<&dyn OpInterfaceWrapper> {
        self.interfaces.get(&uid).map(|v| &**v)
    }

    // Returns true if the op implementation the interface `uid`.
    pub fn has_interface(&self, uid: OpInterfaceUID) -> bool {
        self.interfaces.contains_key(&uid)
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
    builtin_interface: Option<BuiltinOpInterfaceImplWrapper>,
    tags: Option<HashSet<OperationTag>>,
    interfaces: HashMap<OpInterfaceUID, Box<dyn OpInterfaceWrapper>>,
}

impl OperationTypeBuilder {
    pub fn new() -> Self {
        Self {
            opname: None,
            uid: None,
            builtin_interface: None,
            tags: None,
            interfaces: HashMap::new(),
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
    pub fn set_builtin_interface<T: BuiltinOpInterfaceImpl + Default + 'static>(&mut self) {
        assert!(self.builtin_interface.is_none());
        let wrapper: T = Default::default();
        let wrapper = BuiltinOpInterfaceImplWrapper::new(Box::new(wrapper));
        self.builtin_interface = Some(wrapper);
    }

    // Set the tags of the current operation.
    pub fn set_tags(&mut self, tags: &[OperationTag]) {
        assert!(self.tags.is_none());
        let mut tags_set = HashSet::new();
        for tag in tags {
            tags_set.insert(*tag);
        }
        self.tags = Some(tags_set);
    }

    pub fn add_interface<T: OpInterfaceBuilder>(&mut self) {
        let uid = T::InterfaceObjectType::get_interface_uid();
        assert!(
            !self.interfaces.contains_key(&uid),
            "interface redefinition"
        );
        self.interfaces.insert(uid, T::build_interface_object());
    }

    pub fn build(self) -> OperationTypeInfos {
        let opname = self.opname.expect("Missing `opname` value");
        let uid = self.uid.expect("Missing `uid` value");
        let builtin_interface = self
            .builtin_interface
            .expect("Missing `builtin_interface` value");
        let tags = self.tags.unwrap_or(HashSet::new());
        OperationTypeInfos::new(uid, opname, builtin_interface, tags, self.interfaces)
    }
}
