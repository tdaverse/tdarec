## version 0.2.1

This patch is in response to a change in {dials}; see PR #25.

There are no reverse dependencies on CRAN or Bioconductor.

## R CMD checks

### Test environments

All local tests used {TDAvec} version 0.1.41.

* local OS X install, R 4.2.3
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
  * `devtools::check(manual = TRUE, remote = TRUE)`
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
