# This script compiles the time-costly vignette prior to build,
# in order to limit CRAN checktime. It follows rOpenSci guidance:
# https://ropensci.org/blog/2019/12/08/precompute-vignettes/

# knit the static vignette
knitr::knit(
  input  = "vignettes/hyperparameter-tuning-orig.Rmd",
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
