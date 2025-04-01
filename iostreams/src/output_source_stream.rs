enum WriterImpl {
    AnyWriter(Box<dyn std::io::Write>),
    Buffer(Vec<u8>),
}

// Helper class to generate any writer object to write source code.
pub struct OutputSourceStream {
    _impl: WriterImpl,
}

impl OutputSourceStream {
    // Build a stream from any writer object.
    pub fn make_writer(writer: Box<dyn std::io::Write>) -> Self {
        Self {
            _impl: WriterImpl::AnyWriter(writer),
        }
    }

    // Build a stream that will build the buffer object.
    pub fn make_buffer_builder() -> Self {
        Self {
            _impl: WriterImpl::Buffer(Vec::new()),
        }
    }

    // Take the buffer built by the writer if there is one.
    pub fn take_output_buffer(self) -> Option<Vec<u8>> {
        match self._impl {
            WriterImpl::AnyWriter(_) => None,
            WriterImpl::Buffer(buffer) => Some(buffer),
        }
    }

    // Take the string built by the writer if there is one.
    // Panics if the output isn't UTF-8.
    pub fn take_output_string(self) -> Option<String> {
        Some(String::from_utf8(self.take_output_buffer()?).expect("output isn't ASCII"))
    }

    // Returns the writer to write to
    pub fn os(&mut self) -> &mut dyn std::io::Write {
        match &mut self._impl {
            WriterImpl::AnyWriter(os) => os,
            WriterImpl::Buffer(buffer) => buffer,
        }
    }
}
