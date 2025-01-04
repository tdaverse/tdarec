#' @title Standard deviation of Gaussian blur
#' 
#' @description
#' The standard deviation of the noise function convolved with array values to
#' induce blur in lattice data (images).
#'
#' @details
#' Additional details...
#' 
#' @importFrom scales transform_log10
#' @inheritParams dials::Laplace
#' @inheritParams dials::finalize
#' @example inst/examples/ex-param-blur-sigma.R
#' @export
blur_sigma <- function(range = c(0, unknown()), trans = transform_log10()) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(blur_sigma = "Gaussian Blur sigma"),
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
    rlang::abort("The `phom` step can only transform list-columns.")
  }
  
  # # calculate the maximum number of dimensions
  # x_max_dim <- vapply(x, \(l) max(vapply(l, \(m) length(dim(m)), 0L)), 0L)
  # # calculate maximum extent of any dimension
  # x_max_len <- vapply(x, \(l) max(vapply(l, \(m) max(dim(m)), 0L)), 0L)
  # # set the upper bound to one order of magnitude beyond the recommendation
  # rngs[2L] <- x_max_len / 2 ^ (x_max_dim + 1)
  
  # calculate the recommended value for each array
  x_sigmas <- sapply(x, \(l) vapply(
    l,
    \(m) max(dim(m)) / 2 ^ (length(dim(m)) + 1),
    0
  ))
  # set the upper bound to one order of magnitude beyond the maximum
  rngs[2L] <- max(x_sigmas) * 10
  
  if (object$type == "integer" & is.null(object$trans)) {
    rngs <- as.integer(rngs)
  }
  
  dials::range_set(object, rngs)
}
