const fib_rec := func (n: Int) -> Int {
  if n < 2 then { n } else { fib_rec(n - 1) + fib_rec(n - 2) }
};
fib_rec(10);  // 55

const fib := func (n: Int) -> Int {
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
fib(10);  // 55
