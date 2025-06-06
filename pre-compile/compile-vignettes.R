# This script compiles the time-costly vignette prior to build,
# in order to limit CRAN checktime. It follows rOpenSci guidance:
# https://ropensci.org/blog/2019/12/08/precompute-vignettes/
# Note that the initial underscore that flags the source file
# also signals {pkgdown} to ignore it when building articles.

# knit the static vignette
knitr::knit(
  input  = "vignettes/_hyperparameter-tuning.Rmd",
  output = "vignettes/hyperparameter-tuning.Rmd"
)

# obtain image file names
vignette_images <- list.files(
  path = "./",
  pattern = "vignette\\-.*\\.png"
)

# move the images to the `vignettes` folder
file.copy(
  from = vignette_images,
  to   = "vignettes/"
)
file.remove(vignette_images)
