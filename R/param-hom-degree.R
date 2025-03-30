#' @title (Maximum) topological dimension or homological degree
#'
#' @description The degree of the homology group to vectorize, or the degree at
#' which to stop vectorizing.
#'
#' @details Topological features have whole number dimensions that determine the
#' degrees of homology that encode them. Any finite point cloud will have finite
#' topological dimension, but most practical applications exploit features of
#' degree at most 3.
#'
#' Steps may vectorize features of a single degree (`hom_degree()`) or of
#' degrees zero through some maximum (`max_hom_degree()`).
#'
#' In case the (maximum) degree is not provided, `get_hom_range()` queries each
#' list-column for the maximum dimension of its point cloud and returns the
#' smaller of this maximum and `max_dim` (which defaults to `2L`, the highest
#' homological degree of interest in most practical applications).
#'
#' @include vpd-finalizers.R
#' @inheritParams dials::Laplace
#' @inheritParams dials::finalize
#' @inheritParams vpd-finalizers
#' @param max_dim Bound on the maximum dimension determined from the data.
#' @example inst/examples/ex-param-hom-degree.R
#' @export
hom_degree <- function(range = c(0L, unknown()), trans = NULL) {
  new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(hom_degree = "Homological Degree"),
    finalize = get_hom_range
  )
}

#' @rdname hom_degree
#' @export
max_hom_degree <- function(range = c(0L, unknown()), trans = NULL) {
  new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(max_hom_degree = "Maximum Homological Degree"),
    finalize = get_hom_range
  )
}

#' @rdname hom_degree
#' @export
get_hom_range <- function(object, x, max_dim = 2L, ...) {
  check_param(object)
  
  rngs <- dials::range_get(object, original = FALSE)
  if (! dials::is_unknown(rngs$upper)) {
    return(object)
  }
  
  # check that all columns are list-columns of objects with sizes
  if (! all(vapply(x, typeof, "") == "list")) {
    rlang::abort("The `phom` step can only transform list-columns.")
  }
  
  # calculate maximum dimensions of list-columns
  x_max_dims <- vapply(x, \(l) max(vapply(l, ph_dim, 0L), na.rm = FALSE), 0L)
  x_max_dim <- max(x_max_dims)
  
  # set the range based on the maximum observed and the minimum required
  rngs[2L] <- min(max_dim, x_max_dim) - 1L
  
  if (object$type == "integer" & is.null(object$trans)) {
    rngs <- as.integer(rngs)
  }
  
  dials::range_set(object, rngs)
}
