const fib := func (n: Int) -> Int {
  if n < 2 then { n } else { fib(n - 1) + fib(n - 2) }
};
fib(10);  // 55
