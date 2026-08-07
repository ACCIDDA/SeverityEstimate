# Contributing

## Reporting an Issue

Please include:

- A minimal reproducible example or the smallest input that shows the problem.
- What you expected to happen and what happened instead.
- Your `SeverityEstimate` version, R version, and operating system.
- Any relevant error messages, warnings, or traceback output.

`sessionInfo()` is usually enough for version and platform details.

## Contributing Code

### Required Tools

At minimum:

- R (the package currently depends on R >= 4.1.0).
- A working source-package toolchain for `rstan` and C++ compilation.
- `GNU make`.

Useful for local development:

- `just` to run the common project tasks in `justfile`.
- `pak` to install project dependencies (`just deps`).
- `air` and `jarl` if you want to run the same formatting and lint checks as CI.

When developing code locally you can run `just` to perform the common code quality/linting tasks.

### Project Layout

- `R/`: Package source code, roughly with filenames corresponding to the constituent function name.
- `tests/testthat/`: Unit tests, one for each function.
- `inst/stan/`: Stan model files.
- `src/`: Generated Stan/C++ artifacts and compiled sources.
- `man/`: Generated Rd documentation.
- `vignettes/`: Longer-form package documentation in the form of vignettes.
- `inst/notebooks/`: Heavier ad-hoc integration notebooks, such as synthetic parameter-recovery checks, that are intended for local developer use rather than CI.
- `.github/`: CI configuration and workflows.

### Opening a Pull Request

Before opening a PR, it is useful to run the relevant local checks. You can run `just` for the most commont tasks, but it is also helpful to run `just ci` to replicate the CI checks and `just check` to ensure that `R CMD check` runs cleanly (this can take a minute to run).

For changes that affect the statistical model, Stan program, fitting workflow, or downstream parameter extraction, developers should also render the relevant notebooks under `inst/notebooks/`. These notebooks are intentionally ad-hoc and are not currently part of CI, but they should still be used to confirm that the model can recover parameters from synthetic data after substantive model changes. For example, run `just notebook synthetic-parameter-recovery`.

Pull requests should be opened against the `main` branch. On pull requests, CI currently checks:

- Stan-generated artifacts are up to date.
- Roxygen documentation is up to date and committed.
- Formatting and linting.
- Unit tests.
- `R CMD check`.
- `NEWS.md` was updated, unless the commit history explicitly says `no major changes`.
