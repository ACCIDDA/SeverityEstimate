PKG := SeverityEstimate
RM := rm -f
RMDIR := rm -rf

.PHONY: all clean docs lint test test-fast renv-install renv-update renv-snapshot install remove check

all: clean docs lint test

clean:
	$(RMDIR) ..Rcheck/
	$(RMDIR) .Rproj.user/

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

test-fast:
	Rscript -e "library(devtools); \
		devtools::test(stop_on_failure=TRUE);"

renv-install:
	Rscript -e "library(renv); \
		renv::install(dependencies='most');"

renv-update:
	Rscript -e "library(renv); \
		renv::install(dependencies='most'); \
		renv::update();"

renv-snapshot:
	Rscript -e "library(renv); \
		renv::install(dependencies='most'); \
		renv::update(); \
		renv::snapshot();"

install: renv-install
	R CMD INSTALL .

remove:
	R CMD REMOVE $(PKG)

check:
	R CMD check . --no-manual --no-tests
