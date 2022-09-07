all: initc data docs test check

initc:
	R --slave -e "Rcpp::compileAttributes()"
	R --slave -e "tools::package_native_routine_registration_skeleton('.', 'src/init.c', character_only = FALSE)"

docs: man readme vigns site

data:
	Rscript --slave inst/extdata/simulate_data.R

man:
	R --slave -e "devtools::document()"

readme:
	R --slave -e "rmarkdown::render('README.Rmd')"

vigns:
	R --slave -e "devtools::build_vignettes()"
	cp -Rf doc inst/
	touch inst/doc/.gitkeep

quicksite:
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = TRUE)"
	cp -Rf doc inst/
	touch inst/doc/.gitkeep

site:
	R --slave -e "pkgdown::clean_site()"
	R --slave -e "pkgdown::build_site(run_dont_run = TRUE, lazy = TRUE)"
	cp -Rf doc inst/
	touch inst/doc/.gitkeep

test:
	R --slave -e "devtools::test()" > test.log 2>&1
	rm -f tests/testthat/Rplots.pdf

quickcheck:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1
	cp -Rf doc inst/
	touch inst/doc/.gitkeep

check:
	echo "\n===== R CMD CHECK =====\n" > check.log 2>&1
	R --slave -e "devtools::check(remote = TRUE, build_args = '--no-build-vignettes', args = '--no-build-vignettes', run_dont_test = TRUE, vignettes = FALSE)" >> check.log 2>&1
	cp -Rf doc inst/
	touch inst/doc/.gitkeep

wbcheck:
	R --slave -e "devtools::check_win_devel()"
	cp -Rf doc inst/

solarischeck:
	R --slave -e "rhub::check(platform = 'solaris-x86-patched', email = 'jeffrey.hanson@uqconnect.edu.au', show_status = FALSE)"

asancheck:
	R --slave -e "rhub::check(platform = 'linux-x86_64-rocker-gcc-san', email = 'jeffrey.hanson@uqconnect.edu.au', show_status = FALSE)"

build:
	R --slave -e "devtools::build()"
	cp -Rf doc inst/
	touch inst/doc/.gitkeep

quickbuild:
	R --slave -e "devtools::build(vignettes = FALSE)"
	cp -Rf doc inst/

urlcheck:
	R --slave -e "devtools::document();urlchecker::url_check()"

spellcheck:
	R --slave -e "devtools::document();devtools::spell_check()"

examples:
	R --slave -e "devtools::run_examples(run_donttest = TRUE, run_dontrun = TRUE);warnings()" > examples.log 2>&1
	rm -f Rplots.pdf

install:
	R --slave -e "devtools::install_local('.', force = TRUE, upgrade = 'never')"

.PHONY: initc data docs readme site test check checkwb build install man
