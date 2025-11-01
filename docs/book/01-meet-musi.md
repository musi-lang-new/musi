# Chapter 1 · Meet Musi

Musi is a systems language that aims for readable code, predictable memory, and a welcoming learning path. You can think of it as Swift's clarity meeting TypeScript's gradual typing, with low-level control kept within reach.

## First Contact

```musi
const greeting := "Hello, Musi!";
writeln(greeting);
```

This tiny script introduces Musi's two assignment forms: `const` for stable names and `:=` as declaration marker. Musi borrows Swift's strong emphasis on immutability [1](#ref-ch1-swift-declarations) while keeping TypeScript's friendly string handling [2](#ref-ch1-ts-everyday).

## Mindset and Goals

- Use value semantics by default so data stays predictable.
- Reach for optional and expect types instead of nulls, similar to Swift optionals and TypeScript union types [3](#ref-ch1-swift-optionals) [4](#ref-ch1-ts-union).
- Prefer expressions that return values, meaning blocks often end with useful results instead of plain statements.

## Familiar Parallels

| Idea | Musi | Swift | TypeScript |
|------|------|-------|------------|
| Immutable name | `const name := "Ada";` | `let name = "Ada"` | `const name = "Ada"` |
| Mutable update | `var count := 0; count <- 1;` | `var count = 0; count = 1` | `let count = 0; count = 1` |
| Optional check | `if case .Some(const value) := maybe { ... }` | `if let value = maybe { ... }` | `if (value !== undefined) { ... }` |

## Where We Are Going

Next chapter explains declarations and blocks in more depth, showing why Musi separates `:=` from `<-` and how expression blocks tidy control flow. Along the way you will see pattern matching used in loops and conditionals, echoing Swift's pattern support but with Musi's unified syntax.

## Recap

- Musi encourages immutable bindings using familiar keywords.
- Optionals and expect types remove null-centric control flow.
- Syntax keeps surface similarities with Swift and TypeScript while clarifying assignment intent.

Continue to [Chapter 2 · Bindings and Blocks](02-bindings-and-blocks.md).

## References

- <a id="ref-ch1-swift-declarations"></a>[1] Swift Programming Language – Declarations — <https://docs.swift.org/swift-book/documentation/the-swift-programming-language/declarations/>
- <a id="ref-ch1-ts-everyday"></a>[2] TypeScript Handbook – Everyday Types — <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html>
- <a id="ref-ch1-swift-optionals"></a>[3] Swift Programming Language – Optionals — <https://docs.swift.org/swift-book/documentation/the-swift-programming-language/thebasics/#Optionals>
- <a id="ref-ch1-ts-union"></a>[4] TypeScript Handbook – Union Types — <https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#union-types>
