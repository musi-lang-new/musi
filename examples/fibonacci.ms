const fib := func (n: Int) -> Int {
  if n < 2 then { n } else { fib(n - 1) + fib(n - 2) }
};
fib(10);  // 55

const fib_iter := func (n: Int) -> Int {
  if n <= 1 then {
    return n;
  };
  var (a, b) := (0, 1);
  for _ in 2..n {
    const temp := b;
    b <- a + b;
    a <- temp;
  };
  b
};
fib_iter(10);  // 55
