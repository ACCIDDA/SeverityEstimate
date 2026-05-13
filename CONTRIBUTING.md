# Contributing

## Reporting an Issue

Please include:

- A minimal reproducible example or the smallest input that shows the
  problem.
- What you expected to happen and what happened instead.
- Your `SeverityEstimate` version, R version, and operating system.
- Any relevant error messages, warnings, or traceback output.

[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) is usually
enough for version and platform details.

## Contributing Code

### Required Tools

At minimum:

- R (the package currently depends on R \>= 4.1.0).
- A working source-package toolchain for `rstan` and C++ compilation.
- `GNU make`.

Useful for local development:

- `just` to run the common project tasks in `justfile`.
- `renv` to install project dependencies.
- `air` and `jarl` if you want to run the same formatting and lint
  checks as CI.

When developing code locally you can run `just` to perform the common
code quality/linting tasks.

### Project Layout

- `R/`: Package source code, roughly with filenames corresponding to the
  constituent function name.
- `tests/testthat/`: Unit tests, one for each function.
- `inst/stan/`: Stan model files.
- `src/`: Generated Stan/C++ artifacts and compiled sources.
- `man/`: Generated Rd documentation.
- `vignettes/`: Longer-form package documentation in the form of
  vignettes.
- `.github/`: CI configuration and workflows.

### Opening a Pull Request

Before opening a PR, it is useful to run the relevant local checks. You
can run `just` for the most commont tasks, but it is also helpful to run
`just ci` to replicate the CI checks and `just check` to ensure that
`R CMD check` runs cleanly (this can take a minute to run).

Pull requests should be opened against the `main` branch. On pull
requests, CI currently checks:

- Stan-generated artifacts are up to date.
- Roxygen documentation is up to date and committed.
- Formatting and linting.
- Unit tests.
- `R CMD check`.
- `NEWS.md` was updated, unless the commit history explicitly says
  `no major changes`.
