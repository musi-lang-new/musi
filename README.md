# Musi

> [!WARNING]
> Musi is under active development. Language design and implementation are subject to change.

Systems programming language designed for clarity, safety, and performance. Readable syntax meets explicit ARC memory control.

## Highlights

- Expression-oriented semantics; every construct yields a value
- Strong static typing with inference plus gradual escape hatches
- ARC by default with manual control when needed
- Exhaustive pattern matching with guard support
- Fallible types (`T!E`) and structured error propagation

## Example

```musi
record Counter {
  var value: Nat;

  proc inc(var self) {
    self.value <- self.value + 1;
  }

  proc show(self) {
    writeln(`Counter is now: ${self.value}`);
  }
}

var counter := Counter.{ value := 0 };
counter.inc();      // value is now 1
counter.inc();      // value is now 2
counter.show();     // prints: `Counter is now: 2`
```

## Getting Started

### Prerequisites

- [OCaml](https://ocaml.org/install) (5.3.0 or later)
- [opam](https://opam.ocaml.org/doc/Install.html) (OCaml package manager)

### Build

```bash
git clone https://github.com/musi-lang/musi.git
cd musi
opam exec -- dune pkg lock
opam exec -- dune build
```

### Run & Test

```bash
opam exec -- dune exec bin/main.exe
opam exec -- dune test
```

## Contributing

Contributions welcome. See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on submitting pull requests, code standards, and development setup.

## Code of Conduct

All contributors must follow [Code of Conduct](CODE_OF_CONDUCT.md).

## License

See [LICENSE](LICENSE) for details.
