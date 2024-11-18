doc:
	R -e "devtools::document()"

check:
	R -e "devtools::document()"
	R CMD build .
	R CMD check $(shell ls -1t *.tar.gz | head -n 1)

check-as-cran:
	R -e "devtools::document()"
	R CMD build .
	R CMD check --as-cran $(shell ls -1t *.tar.gz | head -n 1)

.PHONY: doc check
