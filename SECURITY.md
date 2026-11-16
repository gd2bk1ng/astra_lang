# Security Policy

SyntraLang is pre-alpha and is not suitable as a security boundary or as a runtime for untrusted programs.

## Reporting a vulnerability

Please do not publish exploit details in a public issue before the maintainer has had an opportunity to assess the report.

If GitHub private vulnerability reporting is enabled for this repository, use the **Report a vulnerability** option in the repository's Security tab. If that option is unavailable, contact the maintainer through the GitHub account associated with this repository and request a private reporting channel.

Include the affected commit/release, component, reproduction steps, impact, attacker assumptions, and any suggested mitigation. Do not include unrelated personal data, credentials, access tokens, or production secrets.

## Security-sensitive areas

- invocation identity and registry isolation;
- owner identity and trusted bootstrap;
- capability issuance, transfer, consumption, and revocation;
- atomicity of state transitions;
- parser and AST source-span integrity;
- semantic verifier soundness;
- host effects and any future execution backend.

## Current limitations

The current crate does not provide a complete sandbox, interpreter, capability-enforcement runtime, or authorization service. Source-level governance and approval fields are not authorization proofs. Do not rely on them to protect resources.

## Supported versions

There are no stable supported releases yet. Report issues against the current `main` branch and identify the exact commit.
