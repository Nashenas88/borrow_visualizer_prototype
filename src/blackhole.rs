use std::io::Write;
use std::io::Result;

// Takes buffers and never outputs them
pub struct BlackHole {}

impl Write for BlackHole {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        // pretend we wrote everything or the compiler will error out
        Ok(buf.len())
    }

    fn flush(&mut self) -> Result<()> {
        Ok(())
    }
}

unsafe impl Send for BlackHole {}