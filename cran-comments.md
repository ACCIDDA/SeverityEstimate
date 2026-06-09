## Resubmission

This is a resubmission addressing reviewer feedback:

* Added `old <- options(mc.cores = 1L)` / `on.exit(options(old))` in the
  `setup` chunk of all three vignettes (`getting-started`, `model-explainer`,
  `mers-korea-2015`) to restore the user's `mc.cores` option after each
  vignette runs.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
