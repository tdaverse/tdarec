#' @title Standard deviation of Gaussian blur
#' 
#' @description The standard deviation of the noise function convolved with
#'   array values to induce blur in raster data.
#'
#' @details The gaussian blur step deploys [blur()]. See there for definitions
#'   and references.
#'
#'   `get_blur_range()` varies the parameter logarithmically from 0 to an order
#'   of magnitude greater than the [blur()] default.
#'
#' @importFrom scales transform_log1p
#' @inheritParams dials::Laplace
#' @inheritParams dials::finalize
#' @returns A `param` object or list of `param` objects.
#' @example inst/examples/ex-param-blur-sigmas.R
#' @export
blur_sigmas <- function(range = c(unknown(), unknown()), trans = transform_log1p()) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(blur_sigmas = "Gaussian Blur std. dev.s"),
    finalize = get_blur_range
  )
}

#' @rdname hom_degree
#' @export
get_blur_range <- function(object, x, ...) {
  check_param(object)
  
  rngs <- dials::range_get(object, original = FALSE)
  if (! dials::is_unknown(rngs$upper)) {
    return(object)
  }
  
  # check that all columns are list-columns of objects with sizes
  if (! all(vapply(x, typeof, "") == "list")) {
    rlang::abort("The `pd` steps can only transform list-columns.")
  }
  
  # # calculate the maximum number of dimensions
  # x_max_dim <- vapply(x, function(l) max(vapply(l, function(m) length(dim(m)), 0L)), 0L)
  # # calculate maximum extent of any dimension
  # x_max_len <- vapply(x, function(l) max(vapply(l, function(m) max(dim(m)), 0L)), 0L)
  # # set the upper bound to one order of magnitude beyond the recommendation
  # rngs[2L] <- x_max_len / 2 ^ (x_max_dim + 1)
  
  # calculate the recommended value for each array
  x_sigmas <- sapply(x, function(l) vapply(
    l,
    function(m) max(dim(m)) / 2 ^ (length(dim(m)) + 1),
    0.
  ))
  # set the lower & upper bounds to one order of magnitude beyond the extrema
  if (dials::is_unknown(rngs$lower)) {
    rngs[1L] <- log1p(min(x_sigmas)) - 1
  }
  if (dials::is_unknown(rngs$upper)) {
    rngs[2L] <- log1p(max(x_sigmas)) + 1
  }
  
  if (object$type == "integer" & is.null(object$trans)) {
    rngs <- as.integer(rngs)
  }
  
  dials::range_set(object, rngs)
}
