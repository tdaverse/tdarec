# This script compiles the time-costly vignette prior to build,
# in order to limit CRAN checktime. It follows rOpenSci guidance:
# https://ropensci.org/blog/2019/12/08/precompute-vignettes/

knitr::knit(
  "vignettes/hyperparameter-tuning-orig.Rmd",
  output = "vignettes/hyperparameter-tuning.Rmd"
)
