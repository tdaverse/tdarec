## First submission

This is the first submission of {tdarec} to CRAN.
Version numbers were not incremented during initial development.
There are no reverse dependencies on CRAN or Bioconductor.

## Long-running vignette

The vignette "Tuning persistent homological hyperparameters" takes several minutes to run.
This could be prevented by shrinking the data set being used, but i believe that the results would then be less illustrative.
The longest-running code chunk conducts Bayesian tuning and is designed to abbreviate at 5 minutes; the other code chunks take less than a minute on my oldest machine.

## R CMD checks

### Test environments

* local OS X install, R 4.2.3, with {TDAvec} 0.1.3 (GitHub; in development beyond 0.1.4)
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
* local OS X install, R 4.4.2, with {TDAvec} 0.1.4 (CRAN)
  * `devtools::check()`
  * `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`
* Win-Builder
  * `devtools::check_win_oldrelease()`
  * `devtools::check_win_release()`
  * `devtools::check_win_devel()`

### local results

There were no ERRORs or WARNINGs.
An occasional and inconsistent NOTE was presumably due to Internet connection speeds.

OS X, R 4.2.3, `devtools::check()`:

```
── R CMD check results ─────────────────────────────────────── tdarec 0.0.1 ────
Duration: 5m 5.4s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

OS X, R 4.2.3, `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`:

```
── R CMD check results ─────────────────────────────────────── tdarec 0.0.1 ────
Duration: 5m 43.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

OS X, R 4.4.2, `devtools::check()`:

```
── R CMD check results ─────────────────────────────────────── tdarec 0.0.1 ────
Duration: 3m 57.7s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

OS X, R 4.4.2, `devtools::check(env_vars = c('_R_CHECK_DEPENDS_ONLY_' = "true"))`:

```
── R CMD check results ─────────────────────────────────────── tdarec 0.0.1 ────
Duration: 4m 6.4s

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

### Win-Builder results

In addition to the package being a new submission, these checks NOTEd the word "vectorization", which is used throughout the package. It is spelled as intended and consistent with common dictionary spellings.

`devtools::check_win_oldrelease()`:

```
Maintainer: 'Jason Cory Brunson <cornelioid@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  Vectorizations (3:9)
  vectorization (12:17)
  vectorizations (14:42)
```

`devtools::check_win_release()`:

```
Maintainer: 'Jason Cory Brunson <cornelioid@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  Vectorizations (3:9)
  vectorization (12:17)
  vectorizations (14:42)
```

`devtools::check_win_devel()`:

```
Maintainer: 'Jason Cory Brunson <cornelioid@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  Vectorizations (3:9)
  vectorization (12:17)
  vectorizations (14:42)
```

