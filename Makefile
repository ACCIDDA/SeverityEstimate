PKG := SeverityEstimate
RM := rm -f
RMDIR := rm -rf

.PHONY: clean docs lint test renv-install renv-update renv-snapshot install remove

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

all: clean docs lint test
