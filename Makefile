PKG := SeverityEstimate
VERSION := $(shell grep -E '^Version:' DESCRIPTION | cut -d ' ' -f 2)
TARBALL := $(PKG)_$(VERSION).tar.gz
RM := rm -f
RMDIR := rm -rf

.PHONY: all
all: clean format lint docs test

.PHONY: clean
clean:
	$(RM)    *.tar.gz
	$(RMDIR) ..Rcheck/
	$(RMDIR) .Rproj.user/

.PHONY: docs
docs:
	Rscript -e "library(roxygen2); \
		roxygen2::roxygenize();"

.PHONY: format
format:
	air format .

.PHONY: lint
lint:
	air format . --check

.PHONY: test
test:
	Rscript -e "library(devtools); \
		devtools::test();"

.PHONY: test-fast
test-fast:
	Rscript -e "library(devtools); \
		devtools::test(stop_on_failure=TRUE);"

.PHONY: renv-install
renv-install:
	Rscript -e "library(renv); \
		renv::install(dependencies='most');"

.PHONY: renv-update
renv-update:
	Rscript -e "library(renv); \
		renv::install(dependencies='most'); \
		renv::update();"

.PHONY: renv-snapshot
renv-snapshot:
	Rscript -e "library(renv); \
		renv::install(dependencies='most'); \
		renv::update(); \
		renv::snapshot();"

.PHONY: install
install: renv-install
	R CMD INSTALL .

.PHONY: remove
remove:
	R CMD REMOVE $(PKG)

.PHONY: build
build:
	R CMD build .

.PHONY: check
check: build
	R CMD check $(TARBALL) --no-manual --no-tests
