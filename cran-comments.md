## version 0.2.0

This minor version (rather than a patch) is being sooner than planned for two reasons:

1. It makes a change to column transformation naming conventions, recommended by a Tidymodels maintainer, that introduces a breaking change.
2. It revises the taking of minima and maxima for grid defaults to exclude infinities, precluding a bug that would be incurred with an upcoming upgrade to the `Suggests` dependency {ripserr}.

There are no reverse dependencies on CRAN or Bioconductor.

## Long-running vignette

The vignette is pre-built to avoid inflating checktime.

## R CMD checks

### Test environments

All local tests used {TDAvec} version 0.1.41.

* local OS X install, R 4.2.3
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
* local OS X install, R 4.4.2
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

### Win-Builder results

There were no ERRORs, WARNINGs, or NOTEs.
