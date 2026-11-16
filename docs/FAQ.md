# SyntraLang FAQ

## Is SyntraLang a complete programming language today?

No. It is a pre-alpha language foundation. The lexer, initial parser/AST, diagnostics, semantic-verification foundations, and invocation coordinator exist. A complete compiler, interpreter, and runtime are not available.

## Is SyntraLang a replacement for Rust, Python, or another general-purpose language?

No such claim is made. SyntraLang explores explicit intent and cognitive-system constructs while retaining ordinary programming-language structures. Its long-term role is still being defined.

## Does it execute programs?

Not currently. The parser builds an AST. The semantic verifier checks a bounded subset of rules. There is no complete execution backend.

## Does `governance { approval reviewer }` authorize an action?

No. It creates syntax/AST metadata. Authentication, approval provenance, authorization, and runtime enforcement are separate systems that are not complete.

## Does `effects { Network }` grant network access?

No. It records a declared effect name. It does not grant operating-system or network permissions.

## Is the lexer/parser zero-allocation?

No. Source lexemes and many AST source fields are borrowed, but token, AST, and diagnostic collections allocate. The current crate uses `std`.

## Is the grammar stable?

No. The repository documents the current parser subset, but pre-1.0 syntax and APIs may change.

## Can I use it in production?

It is not recommended for production workloads or security-critical decisions. It is an evolving research and engineering project.

## How do I build and test it?

Follow [Getting Started](GETTING_STARTED.md) and run the Cargo checks listed there.

## How can I contribute?

Read the repository's [Contributing Guide](../CONTRIBUTING.md), choose a focused issue, and submit a pull request against `main`.

## How does SyntraLang relate to Syntra Kernel?

SyntraLang is intended to remain independently usable. Integration with Syntra Kernel is a future project milestone, not a completed feature.

## Why is there historical Astra material?

Earlier Astra work contains research and experiments. It is retained as historical source material; SyntraLang's current contracts and architecture are authoritative for active development.
