doc:
	R -e "devtools::document()"

check: doc
	R CMD build .
	R CMD check $(shell ls -1t *.tar.gz | head -n 1)

check-as-cran: doc
	R CMD build .
	R CMD check --as-cran $(shell ls -1t *.tar.gz | head -n 1)

test: doc
	R -e "devtools::test()"

install: doc
	R -e "devtools::install_local(force = TRUE, upgrade = 'never')"

.PHONY: doc check install test check-as-cran
