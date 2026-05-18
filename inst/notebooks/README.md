# Notebooks

These R Markdown notebooks are heavier than the package vignettes and are intended for local, integration-style checks that may require longer Stan runs. They are meant to be rendered from a source checkout and use `devtools::load_all()` to exercise the current working tree.

Render them from the repository root with:

```bash
just notebook synthetic-parameter-recovery
```

Rendered outputs are written to `inst/notebooks/rendered/`, which is ignored by git and excluded from `R CMD build`.

When the package is installed, notebook sources are still available under:

```r
system.file("notebooks", package = "SeverityEstimate")
```
