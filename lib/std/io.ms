unsafe extern "intrinsic" proc __builtin_writeln(text: Text);
unsafe extern "intrinsic" proc __builtin_write(text: Text);

export {
  __builtin_writeln as writeln,
  __builtin_write as write
};
