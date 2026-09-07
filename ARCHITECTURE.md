# kaefa Architecture

Last updated: 2026-02-14

## Purpose

`kaefa` is an R package for automated exploratory factor analysis (AEFA).
It provides:

- core AEFA execution (`aefa`, `engineAEFA`),
- optional remote worker initialization (`aefaInit`),
- an interactive Shiny UI (`launchAEFA`).

## Productization Boundaries

The repository remains a monorepo until the product surface has independent
release or deployment needs. Use these boundaries when planning sale-readiness
work:

- `kaefa-core`: the R/statistical engine boundary around `aefa()`,
  `engineAEFA()`, model selection, item-fit evaluation, theta-prior utilities,
  and benchmark evidence.
- `kaefa-studio`: the buyer-facing UI boundary around `launchAEFA()` and
  `inst/shiny-app/app.R`.
- `kaefa-runner`: the future deployment and execution boundary for container,
  hosted, remote, or scheduled analysis workflows.

Do not introduce a git submodule unless a downstream buyer or deployment model
explicitly requires vendored source integration.

## Repository Layout

- `R/kaefa.R`: public orchestration entry points and exported runtime behavior.
- `R/newEngine.R`: candidate-model estimation engine used by the AEFA loop.
- `R/utils.R`: helper routines and shared utilities.
- `inst/shiny-app/app.R`: bundled Shiny interface logic.
- `inst/shiny-app/README.md`: Shiny usage and minimal UI configuration guide.
- `tests/testthat/*.R`: functional, regression, and integration tests.
- `.github/workflows/R-CMD-check.yaml`: required multi-OS package checks.
- `.github/workflows/dependency-review.yml`: dependency risk gate.
- `README.Rmd` -> `README.md`: source and generated top-level documentation.

## Runtime Flow

1. User calls `aefa()` or launches `launchAEFA()`.
2. `aefa()` coordinates iterative model search and candidate evaluation.
3. `engineAEFA()` performs lower-level model estimation for each candidate.
4. Best model is selected by configured information criteria and returned.
5. Optional history/diagnostics are exposed when enabled.

## Optional Remote Execution

- `aefaInit()` configures worker hosts and SSH key paths.
- Remote usage is optional; local execution is the default path.
- Security-sensitive values (keys/tokens) must remain out of git history.

## Quality and Security Gates

- PR merge requires review approval and resolved conversations.
- Organization-owned required review and security gates run on every PR through
  reusable workflows in `ContextualWisdomLab/.github`.
- Local `R-CMD-check`, `test-fast`, and `test-suite` workflows run for runtime,
  package, workflow, `README.Rmd`, and public API contract changes. Plain
  Markdown changes are excluded, except
  `docs/product/kaefa-core-api-contract.md`, because
  `tests/testthat/test-core-api-contract.R` consumes that file as executable
  contract input.
- `tests/testthat/test-workflow-path-contract.R` prevents the API-contract
  exception from silently disappearing when the trigger paths change.

## Change Rule

When architecture-level behavior changes (entry points, runtime flow, or CI
gates), update this document in the same change set.
