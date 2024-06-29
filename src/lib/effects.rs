pub trait EffectSink {
    fn emit_encoded_error(&mut self, returncode: i32, bytes: &[u8]);
    fn emit_encoded_errors(&mut self, returncode: i32, byte_sequewnces: &[&[u8]]);
    fn emit_encoded_output(&mut self, bytes: &[u8]);
    fn flush(&mut self);
    fn deferred_exit_code(&self) -> i32;
}
