// This struct is used to write to output, and implement the write parameter.
// Internally it's calling print! maccro.
// It's slightly different than using &mut stdout(), as this doesn't take any lock.
// And in some cases stdout stream differs from print!();
#[derive(Clone, Copy, Default)]
pub struct StdoutWriter;

impl std::io::Write for StdoutWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let str = std::str::from_utf8(buf).expect("Only support UTF-8 output encoding");
        print!("{}", str);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
