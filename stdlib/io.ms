const __builtin_writeln := unsafe extern "intrinsic" proc (s: Text);
const __builtin_write := unsafe extern "intrinsic" proc (s: Text);

/// Writes given text followed by newline to standard output.
proc writeln(s: Text) { __builtin_writeln(s); }

/// Writes given text to standard output.
proc write(s: Text) { __builtin_write(s); }

export { writeln, write};
