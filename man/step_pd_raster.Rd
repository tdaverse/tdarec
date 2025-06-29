% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step-pd-raster.R
\name{step_pd_raster}
\alias{step_pd_raster}
\alias{tidy.step_pd_raster}
\title{Persistent homology of raster data (images)}
\usage{
step_pd_raster(
  recipe,
  ...,
  role = NA_character_,
  trained = FALSE,
  filtration = "cubical",
  value_max = 9999L,
  method = c("link_join", "compute_pairs"),
  engine = NULL,
  columns = NULL,
  skip = FALSE,
  id = rand_id("pd_raster")
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

\item{filtration}{The type of filtration from which to compute persistent
homology; currently only \code{"cubical"}.}

\item{value_max, method}{Parameters passed to persistence engines.}

\item{engine}{The computational engine to use (see 'Details'). Reasonable
defaults are chosen based on \code{filtration}.}

\item{columns}{A character string of the selected variable names. This field
is a placeholder and will be populated once \code{\link[recipes:prep]{prep()}} is used.}

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
The function \code{step_pd_raster()} creates a \emph{specification} of a
recipe step that will convert compatible data formats (numerical arrays,
including matrices, of 2, 3, or 4 dimensions) to 3-column matrix
representations of persistence diagram data. The input and output must be
list-columns.
}
\details{
Persistent homology (PH) is a tool of algebraic topology to extract features
from data whose \emph{persistence} measures their robustness to scale. The
computation relies on a sequence of maps between discrete topological spaces
(usually a filtration comprising only inclusions) constructed from the data.
}
\section{PH of Rasters}{


The PH of numeric arrays such as (greyscale) digital images is computed
from the cubical filtration of the pixel or voxel array, treated as a
function from a cubical mesh to a finite value range.

Cubical Ripser is an efficient implementation of cubical PH and is ported
to R through \strong{\link[ripserr:ripserr]{ripserr}}. It accepts numerical
arrays.

The \code{value_max} argument bounds the value range along which PH is computed.
Cubical Ripser is implemented using both of two methods, link-join and
compute-pairs, controlled by the \code{method} parameter.
}

\section{Tuning Parameters}{


This step has 1 tuning parameter(s):
\itemize{
\item \code{max_hom_degree}: Maximum Homological Degree (type: integer, default: NULL)
}
}

\examples{
topos <- data.frame(pix = I(list(volcano)))

ph_rec <- recipe(~ ., data = topos) \%>\% 
  step_pd_raster(pix)
ph_prep <- prep(ph_rec, training = topos)
ph_res <- bake(ph_prep, topos)

tidy(ph_rec, number = 1)
tidy(ph_prep, number = 1)

with(ph_res$pix[[1]], plot(
  x = birth, y = death, pch = dimension + 1, col = dimension + 1,
  xlab = NA, ylab = "", asp = 1
))

with_max <- recipe(~ ., data = topos) \%>\% 
  step_pd_raster(pix, value_max = 150)
with_max <- prep(with_max, training = topos)
bake(with_max, topos)
}
\seealso{
Other topological feature extraction via persistent homology: 
\code{\link{step_pd_degree}()},
\code{\link{step_pd_point_cloud}()}
}
\concept{topological feature extraction via persistent homology}
