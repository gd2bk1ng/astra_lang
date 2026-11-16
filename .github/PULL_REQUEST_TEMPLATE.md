## Summary

Describe the change and the problem it solves.

## Design and invariants

List the invariants, trust boundaries, or compatibility decisions affected.

## Verification

- [ ] `cargo fmt --all -- --check`
- [ ] `cargo check --all-targets`
- [ ] `cargo test --all-targets`
- [ ] `cargo clippy --all-targets --all-features -- -D warnings`

Record toolchain, platform, and results. Do not check boxes for commands that were not run.

## Documentation

- [ ] README/reference/architecture/status/roadmap updated where relevant.
- [ ] Examples match the current parser and public API.
- [ ] Planned behavior is clearly labelled.

## Risk review

Describe failure modes, atomicity guarantees, and security implications. For runtime changes, include adversarial tests.
