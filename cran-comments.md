## First submission

This is the first submission of {tdarec} to CRAN.
Version numbers were not incremented during initial development.
There are no reverse dependencies on CRAN or Bioconductor.

## Long-running vignette

The vignette is now pre-built to avoid inflating checktime.

## R CMD checks

### Test environments

* local OS X install, R 4.2.3, with {TDAvec} 0.1.3 (GitHub; in development beyond 0.1.4)
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
* local OS X install, R 4.4.2, with {TDAvec} 0.1.4 (CRAN)
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
  * `devtools::check(manual = TRUE, remote = TRUE)`
* Win-Builder
  * `devtools::check_win_oldrelease()`
  * `devtools::check_win_release()`
  * `devtools::check_win_devel()`

### local results

There were no ERRORs or WARNINGs.
An occasional NOTE was presumably due to Internet connection speeds.

The exception, with CRAN incoming checks and manual build, also NOTEd that this is a new submission.

### Win-Builder results

In addition to the package being a new submission, these checks NOTEd the words "vectorization(s)" and "vectorizing" used throughout the package and the words "Silge", "Tidymodels", "al" (part of "&al"), and "modularizes".
All are spelled as intended.
