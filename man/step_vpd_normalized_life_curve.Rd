% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz-step-vpd-normalized-life-curve.R
\name{step_vpd_normalized_life_curve}
\alias{step_vpd_normalized_life_curve}
\alias{tidy.step_vpd_normalized_life_curve}
\title{Normalized Life Curve Vectorization of Persistent Homology}
\usage{
step_vpd_normalized_life_curve(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  hom_degree = 0L,
  xseq = NULL,
  xmin = NULL,
  xmax = NULL,
  xlen = NULL,
  xby = NULL,
  evaluate = "intervals",
  columns = NULL,
  keep_original_cols = TRUE,
  skip = FALSE,
  id = rand_id("vpd_normalized_life_curve")
)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the sequence of
operations for this recipe.}

\item{...}{One or more selector functions to choose variables for this step.
See \code{\link[recipes:selections]{selections()}} for more details.}

\item{role}{For model terms created by this step, what analysis role should
they be assigned? By default, the new columns created by this step from the
original variables will be used as \emph{predictors} in a model.}

\item{trained}{A logical to indicate if the quantities for preprocessing have
been estimated.}

\item{hom_degree}{The homological degree of the features to be transformed.}

\item{xseq}{A discretization grid, as an increasing numeric vector.
\code{xseq} overrides the other \verb{x*} parameters with a warning.}

\item{xmin, xmax, xlen, xby}{Limits and resolution of a discretization grid;
specify only one of \code{xlen} and \code{xby}.}

\item{evaluate}{The method by which to vectorize continuous functions over a grid,
either 'intervals' or 'points'.
Some functions only admit one method.}

\item{columns}{A character string of the selected variable names. This field
is a placeholder and will be populated once \code{\link[recipes:prep]{prep()}} is used.}

\item{keep_original_cols}{A logical to keep the original variables in the
output. Defaults to \code{FALSE}.}

\item{skip}{A logical. Should the step be skipped when the recipe is baked by
\code{\link[recipes:bake]{bake()}}? While all operations are baked when \code{\link[recipes:prep]{prep()}} is run, some
operations may not be able to be conducted on new data (e.g. processing the
outcome variable(s)). Care should be taken when using \code{skip = TRUE} as it
may affect the computations for subsequent operations.}

\item{id}{A character string that is unique to this step to identify it.}
}
\value{
An updated version of \code{recipe} with the new step added to the
sequence of any existing operations.
}
\description{
The function \code{step_vpd_normalized_life_curve()} creates
a \emph{specification} of a recipe step that will convert
a list-column of 3-column matrices of persistence data
to a list-column of 1-row matrices of vectorizations.
}
\details{
Persistent homology is usually encoded as birth--death pairs (barcodes or
diagrams), but the space of persistence data sets does not satisfy convenient
statistical properties. Such applications as hypothesis testing and machine
learning benefit from transformations of persistence data, often to Hilbert
spaces (vector spaces with inner products and induced metrics).
}
\section{Engine}{


The normalized life curve vectorization deploys
\code{\link[TDAvec:computeNormalizedLife]{TDAvec::computeNormalizedLife()}}.
See there for definitions and references.
}

\section{Tuning Parameters}{


This step has 1 tuning parameter:
\itemize{
\item \code{hom_degree}: Homological degree (type: integer, default: \code{0L})
}
}

\examples{
library(recipes)

# inspect vectorized features
volc_dat <- data.frame(image = I(list(volcano / 10)))
recipe(~ image, data = volc_dat) \%>\% 
  step_pd_raster(image, method = "link_join") \%>\% 
  step_vpd_normalized_life_curve(image, hom_degree = 1) \%>\% 
  print() -> volc_rec
print(volc_rec)
volc_rec \%>\% 
  prep(training = volc_dat) \%>\% 
  bake(new_data = volc_dat)

# dimension-reduce using vectorized features
data(permeability_qsar, package = "modeldata")
permeability_qsar \%>\% 
  transform(perm_cut = cut(permeability, breaks = seq(0, 60, 10))) \%>\% 
  subset(select = -permeability) \%>\% 
  tidyr::nest(chem_fp = -perm_cut) \%>\% 
  print() -> perm_dat
recipe(perm_cut ~ chem_fp, data = perm_dat) \%>\% 
  step_pd_point_cloud(chem_fp, max_hom_degree = 2) \%>\% 
  step_vpd_normalized_life_curve(chem_fp, hom_degree = 1) \%>\% 
  step_pca(starts_with("chem_fp_"), num_comp = 2) \%>\%
  print() -> perm_rec
perm_est <- prep(perm_rec, training = perm_dat)
perm_res <- bake(perm_est, new_data = perm_dat)
# inspect results
tidy(perm_rec)
tidy(perm_rec, number = 2)
tidy(perm_est, number = 2)
# visualize results
with(perm_res, {
  plot(PC1, PC2, type = "n", asp = 1)
  text(PC1, PC2, labels = perm_cut)
})
}
