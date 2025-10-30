# Contributing to Musi

Thanks for your interest in contributing. All contributions welcome, whether written manually or with AI assistance.

## Getting Started

1. Fork repository
2. Clone your fork: `git clone https://github.com/YOUR_USERNAME/musi.git`
3. Create branch: `git checkout -b feat/your-feat-name`
4. Read relevant documentation and existing code
5. Make changes
6. Run tests: `opam exec -- dune test`
7. Commit with clear messages
8. Push and open pull request

## AI Tool Usage

AI coding assistants are welcome. Project includes context files:

- `.github/copilot-instructions.md` for GitHub Copilot
- Other tool-specific directories (check repository)

**Critical**: AI tools are limited and make mistakes. Know what you're doing before using them. You're responsible for code quality, not AI.

**Best practices**:

- Read codebase first
- Understand architecture before asking AI
- Review and test all AI suggestions
- Use AI for boilerplate, not critical logic
- Verify AI code meets project standards

## Code Standards

### Principles

Follow KISS, DRY, and YAGNI:

- **KISS**: Write simplest solution that works, avoid complications
- **DRY**: Extract repeated code immediately, single source of truth
- **YAGNI**: Build only what is needed now, no "future-proofing"

### Naming Conventions

- **Module Types**: `PascalCase` (e.g., `Ast`, `Token`)
- **Functions and Variables**: `snake_case`
- **Type Names**: Use `t` for main type in module (OCaml convention)
- **Variant Constructors**: `PascalCase`

**Descriptive names**:

- Explain intent and reflect return types
- No single-letter names except loop counters
- Use `_` prefix for intentionally unused variables

### OCaml-Specific Guidelines

**Type annotations**:

- Add type annotations when OCaml can't infer (e.g., record construction with shared field names)
- Prefer explicit types for public functions in `.mli` files

**Pattern matching**:

- Use exhaustive pattern matching
- Avoid catch-all `_` patterns when specific cases should be handled
- Use `function` for single-argument pattern matching

**Immutability**:

- Prefer immutable data structures
- Use mutable fields only when necessary (e.g., parser state)
- Mark mutable fields explicitly with `mutable`

**Code quality**:

- Write self-documenting code
- Keep functions small (< 50 lines ideal)
- Avoid deep nesting (max 3-4 levels)
- Add tests for new functionality
- Update documentation when changing behaviour

### Comments

- Document **why**, not **what** (code shows what)
- Only comment when intent is not clear from code
- No commented-out code blocks

## Pull Request Guidelines

- Describe what PR does and why
- Reference related issues
- Ensure all tests pass
- Keep changes focused (one feature or fix per PR)
- Respond to review feedback promptly

## Reporting Issues

When reporting bugs, include:

- Clear description of problem
- Steps to reproduce
- Expected vs actual behaviour
- Musi version and platform

For feature requests, explain use case and why it benefits project.

## Development Setup

### Prerequisites

- OCaml (5.3.0 or later)
- opam (OCaml package manager)
- Dune (build system)
- Git

### Building

```bash
opam exec -- dune pkg lock   # ensure reproducible dependency resolution
opam exec -- dune build      # build all packages
```

### Testing

```bash
opam exec -- dune test                               # run all tests
opam exec -- dune exec lib/frontend/test_lexer.exe   # run specific test
```

### Project Structure

- `lib/shared/` -- Core utilities: Span, Source, Diagnostic, Interner, helpers.
- `lib/frontend/` -- Lexer, Pratt parser, tokens, AST, symbol tables, name resolution, type checker.
- `lib/backend/` -- Instr definitions and emitter.
- `lib/runtime/` -- VM and standard library runtime.
- `bin/` -- Compiler CLI / entry point.
- `test/` -- Integration tests.

## Questions?

Open issue for questions about contributing, code standards, or project direction.

## Code of Conduct

All contributors must follow [Code of Conduct](CODE_OF_CONDUCT.md).
