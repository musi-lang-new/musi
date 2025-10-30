const __builtin_writeln := unsafe extern "intrinsic" proc (s: Text);
const __builtin_write := unsafe extern "intrinsic" proc (s: Text);

export { __builtin_writeln, __builtin_write };
