PKG := SeverityEstimate
RM := rm -f
RMDIR := rm -rf

.PHONY: clean docs lint test renv-install renv-update renv-snapshot

clean:
	$(RMDIR) ..Rcheck/

docs:
	Rscript -e "library(roxygen2); \
		roxygen2::roxygenize();"

lint:
	Rscript -e "library(devtools); \
		library(lintr); \
		devtools::load_all(); \
		lintr::lint_package();"

test:
	Rscript -e "library(devtools); \
		devtools::test();"
		
renv-install:
	Rscript -e "library(renv); \
		renv::install();"

renv-update:
	Rscript -e "library(renv); \
		renv::install(); \
		renv::update();"

renv-snapshot:
	Rscript -e "library(renv); \
		renv::install(); \
		renv::update(); \
		renv::snapshot();"

all: clean docs lint test
