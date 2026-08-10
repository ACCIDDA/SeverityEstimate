PKG := "SeverityEstimate"
VERSION := `sed -n 's/^Version: //p' DESCRIPTION`
TARBALL := PKG + "_" + VERSION + ".tar.gz"

default: clean format lint docs test pkgdown-fast

[unix]
[group('dev')]
[doc('Clean up auxiliary files and directories')]
clean:
	rm -f  *.tar.gz
	rm -rf ./*.Rcheck/
	rm -rf .Rproj.user/
	rm -rf docs/
	rm -f vignettes/*.html
	rm -rf inst/notebooks/rendered

[group('dev')]
[doc('Build man pages using roxygen')]
docs:
	#!/usr/bin/env Rscript
	library(roxygen2)
	roxygen2::roxygenize()

[group('dev')]
[doc('Fail if roxygen documentation changes are not committed')]
docs-check: docs
	@if [ -n "$(git status --porcelain)" ]; then \
		echo "Please run roxygen2::roxygenize() and commit the changes."; \
		exit 1; \
	fi

[group('dev')]
[doc('Regenerate Stan package artifacts under src/ and R/stanmodels.R')]
stan:
	#!/usr/bin/env Rscript
	library(rstantools)
	rstantools::rstan_config()

[group('dev')]
[doc('Fail if Stan-generated package artifacts are out of date')]
stan-check: stan
	@if ! git diff --quiet -- \
		R/stanmodels.R \
		src \
		configure \
		configure.win \
		inst/include/stan_meta_header.hpp; then \
		echo "Stan-generated files are out of date. Run 'just stan' and commit the changes."; \
		git diff -- R/stanmodels.R src configure configure.win inst/include/stan_meta_header.hpp; \
		exit 1; \
	fi

[group('dev')]
[doc('Format R code using air and auto-fix jarl findings when possible')]
format:
	air format .
	jarl check . --fix --allow-dirty

[group('dev')]
[doc('Check R code using air and jarl')]
lint:
	air format . --check
	jarl check .

[group('dev')]
[doc('Run unit tests using devtools')]
test:
	#!/usr/bin/env Rscript 
	library(devtools)
	devtools::test()

[group('dev')]
[doc('Run unit tests using devtools, stopping on first failure')]
test-fast:
	#!/usr/bin/env Rscript
	library(devtools)
	devtools::test(stop_on_failure=TRUE)

[group('dev')]
[doc('Fail if NEWS.md was not updated on non-main branches unless commits say no major changes')]
news-check:
	@BRANCH=$(git rev-parse --abbrev-ref HEAD); \
	if [ "$BRANCH" = "main" ]; then \
		echo "Skipping news check on main"; \
		exit 0; \
	fi; \
	git fetch origin main:main; \
	GIT_LOG=$(git log main..HEAD --pretty=format:"%s %b"); \
	SKIP_REGEX="no[[:space:]]+major[[:space:]]+changes"; \
	if echo "$GIT_LOG" | tr '\n' ' ' | grep -Eiq "$SKIP_REGEX"; then \
		echo "Bypassing news check: 'no major changes' found in commit history"; \
		exit 0; \
	fi; \
	if ! git diff --name-only main...HEAD | grep -q '^NEWS.md$'; then \
		echo "Error: Please update NEWS.md"; \
		exit 1; \
	fi

[group('dev')]
[doc('Run local CI-equivalent checks except R CMD check')]
ci: stan-check docs-check lint test-fast news-check pkgdown

[group('dev')]
[doc('Build a tar.gz artifact')]
build:
	R CMD build .

[group('dev')]
[doc('Check the built tar.gz artifact')]
check: build
	R CMD check {{ TARBALL }} --no-manual --no-tests

[group('deps')]
[doc('Install package dependencies, including Suggests, using pak')]
deps:
	#!/usr/bin/env Rscript
	if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
	pak::local_install_deps(dependencies = TRUE)

[group('install')]
[doc('Install development version of SeverityEstimate')]
install: deps
	R CMD INSTALL .

[group('install')]
[doc('Remove development version of SeverityEstimate')]
remove:
	R CMD REMOVE {{ PKG }}

[group('pkgdown')]
[doc('Build the full pkgdown site locally')]
pkgdown: install
	Rscript -e 'pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)'

[group('pkgdown')]
[doc('Build pkgdown home/reference pages without rendering articles')]
pkgdown-fast: install
	Rscript -e 'pkgdown::build_home(); pkgdown::build_reference()'

[group('pkgdown')]
[doc('Build and open the pkgdown site locally in a browser')]
pkgdown-preview: pkgdown
	Rscript -e 'pkgdown::preview_site()'

[group('vignettes')]
[doc('Render a vignette locally')]
vignette target:
	Rscript -e 'rmarkdown::render("vignettes/{{ target }}.Rmd")'

[group('vignettes')]
[doc('Render a vignette and open the generated HTML in the default browser')]
view target: (vignette target)
	Rscript -e 'utils::browseURL(normalizePath("vignettes/{{ target }}.html"))'

[group('notebooks')]
[doc('Render an integration notebook locally')]
notebook target:
	Rscript -e 'dir.create("inst/notebooks/rendered", recursive = TRUE, showWarnings = FALSE); rmarkdown::render(input = "inst/notebooks/{{ target }}.Rmd", output_file = "{{ target }}.html", output_dir = "inst/notebooks/rendered", clean = TRUE, envir = new.env(parent = globalenv()))'

[group('notebooks')]
[doc('Render an integration notebook and open the generated HTML in the default browser')]
notebook-view target: (notebook target)
	Rscript -e 'utils::browseURL(normalizePath("inst/notebooks/rendered/{{ target }}.html"))'
