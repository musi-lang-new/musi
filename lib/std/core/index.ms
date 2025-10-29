unsafe extern "intrinsic" proc __builtin_panic(msg: Text);
unsafe extern "intrinsic" proc __builtin_assert(cond: Bool, msg: Text);

export {
    __builtin_panic as panic,
    __builtin_assert as assert,
}
