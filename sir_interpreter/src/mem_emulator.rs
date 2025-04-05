/// Basic object to emulate the memory for the interpreter.
pub struct MemEmulator {
    stack_offset: usize,
    stack_size: usize,
    stack: Vec<u8>,
}

impl MemEmulator {
    /// Create a new memory object.
    pub fn new() -> Self {
        let stack_offset = 0xFFFF0000;
        let stack_size = 0xFFFF;
        let mut stack = Vec::new();
        stack.resize(stack_size, 0);

        Self {
            stack_offset,
            stack_size,
            stack,
        }
    }

    /// Returns the address to the start of the stack.
    pub fn stack_address(&self) -> usize {
        self.stack_offset
    }

    /// Returns a mutable reference of type T from its address.
    /// Returns none if the address is invalid.
    /// The function is unsafe as we can't guarantee that the object was allocated before.
    /// Reading the reference is UB.
    pub unsafe fn get_ref<T: Sized>(&self, addr: usize) -> Option<&T> {
        // Get the pointer.
        let (ptr, mem_size) = self._get_ptr(addr)?;

        // Ensure there is enough bytes left in the segment to grab a T.
        let type_size = std::mem::size_of::<T>();
        if type_size > mem_size {
            return None;
        }

        // Here we can safely take a reference.
        let ptr = ptr as *const T;
        Some(unsafe { ptr.as_ref().unwrap() })
    }

    /// Returns a mutable reference of type T from its address.
    /// Returns none if the address is invalid.
    /// The function is unsafe as we can't guarantee that the object was allocated before.
    /// Reading / Writing the reference is UB.
    pub unsafe fn get_mut_ref<T: Sized>(&mut self, addr: usize) -> Option<&mut T> {
        // Get the pointer.
        let (ptr, mem_size) = self._get_mut_ptr(addr)?;

        // Ensure there is enough bytes left in the segment to grab a T.
        let type_size = std::mem::size_of::<T>();
        if type_size > mem_size {
            return None;
        }

        // Here we can safely take a reference.
        let ptr = ptr as *mut T;
        Some(unsafe { ptr.as_mut().unwrap() })
    }

    // Write `val` to memory address `addr`.
    /// Returns none if the address is invalid.
    /// Returns the number of bytes written.
    /// Differs from `self.get_mut_ref(addr) = val``, here the memory doesn't need to be initialized.
    pub fn write_data<T: Sized>(&mut self, addr: usize, val: T) -> Option<usize> {
        // Get the pointer.
        let (ptr, mem_size) = self._get_mut_ptr(addr)?;

        // Ensure there is enough bytes left in the segment to grab a T.
        let type_size = std::mem::size_of::<T>();
        if type_size > mem_size {
            return None;
        }

        // Here we can safely write the value.
        let ptr = ptr as *mut T;
        unsafe {
            std::ptr::write(ptr, val);
        }
        Some(type_size)
    }

    // Returns a ptr as well as the remaining amount of available memory.
    fn _get_ptr(&self, addr: usize) -> Option<(*const u8, usize)> {
        if addr < self.stack_offset {
            // Unmapped memory
            None
        } else if addr >= self.stack_offset && addr < self.stack_offset + self.stack_size {
            let ptr_offset = addr - self.stack_offset;
            let rem_bytes = self.stack_size - ptr_offset;
            let ptr = self.stack.as_ptr().wrapping_add(ptr_offset);
            Some((ptr, rem_bytes))
        } else {
            // Unmapped memory
            None
        }
    }

    // Returns a ptr as well as the remaining amount of available memory.
    fn _get_mut_ptr(&mut self, addr: usize) -> Option<(*mut u8, usize)> {
        if addr < self.stack_offset {
            // Unmapped memory
            None
        } else if addr >= self.stack_offset && addr < self.stack_offset + self.stack_size {
            let ptr_offset = addr - self.stack_offset;
            let rem_bytes = self.stack_size - ptr_offset;
            let ptr = self.stack.as_mut_ptr().wrapping_add(ptr_offset);
            Some((ptr, rem_bytes))
        } else {
            // Unmapped memory
            None
        }
    }
}
