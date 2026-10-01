#' @title Scale Sequences representing by Limit and Resolution of grids
#'
#' @description
#' Here we have lower and upper limit, added to resolution of the grid over 
#' which a persistence diagram-based vectorization is evaluated.
#' 
#' @details #' Vectorization of PDs is often obtained by grids evaluation. The
#' resolution of that grid depends on its limits, just as a larger image
#' doesn't need to be as sharp for example, a grid over a wider range doesn't 
#' need as fine a resolution, so (`xby`) can increase in most cases as the range 
#' expands.
#'
#' In case a limit is not provided, (`get_scale_range()`) provides the
#' training data's own birth-death range and expands it by a buffer(here we 
#' simply double the original range which is not empirically validated), so that 
#' features in testing data falling outside the training range can still be 
#' captured. 
#'
#' In case the resolution (`grid_by()`) is not provided, (`get_scale_increment()`) 
#' uses the data to set it. The minimum observed persistence is used as a lower
#' bound, so the grid can resolve the shortest-lived feature. The overall span
#' divided by 2^3 is used as an upper bound. Candidate resolutions are then
#' tried between these two bounds on a log2 scale and tuning picks the one that 
#' works best.
#' 
#' @inheritParams dials::Laplace
#' @inheritParams dials::finalize
#' 
#' @param hom_degree the homological degree of the features
#' @param bound differ which bound (lower/upper) of the grid to be finalized
#' 
#' @returns A `param` object or list of `param` objects.
#' 
#' @example inst/examples/ex-param-scale-sequences.R
#' 
#' @rdname scale_sequence
#' @export
grid_by <- function(range = c(unknown(), unknown()), trans = scales::transform_log(base = 2)) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(grid_by = "Grid resolution (as power of 2)"),
    finalize = get_scale_increment
  )
}

#' @rdname scale_sequence
#' @export
grid_min <- function(range = c(unknown(), unknown()), trans = NULL) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(grid_min = "Grid lower limit"),
    finalize = function(object, x, ...) get_scale_range(object, x, bound = "lower", ...)
  )
}

#' @rdname scale_sequence
#' @export
grid_max <- function(range = c(unknown(), unknown()), trans = NULL) {
  new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(grid_max = "Grid upper limit"),
    finalize = function(object, x, ...) get_scale_range(object, x, bound = "upper", ...)
  )
}


#' @rdname scale_sequence
#' @export
get_scale_range <- function(object, x, hom_degree = NULL, bound = c("lower", "upper"), ...){
  check_param(object, allow_unknown = TRUE)
  
  rngs <- dials::range_get(object, original = FALSE)
  if (! dials::is_unknown(rngs$upper)) {
    return(object)
  }
  
  if (! all(vapply(x, typeof, "") == "list")) {
    rlang::abort("The `pd` steps can only transform list-columns.")
  }
  
  ranges <- sapply(x, function(l) {
    val <- sapply(l, function(pd) life_support(pd, hom_degree), simplify = TRUE)
    range(val[is.finite(val)])
  })
  r <- c(max(ranges[1L, ]), min(ranges[2L, ]))
  span <- r[2] - r[1]
  low <- if ("lower" %in% bound) r[1] - span else r[2]
  high <- if ("upper" %in% bound) r[2] + span else r[1]
  rngs <- c(low, high)
  dials::range_set(object, rngs)
}
  

#' @rdname scale_sequence
#' @export
get_scale_increment <- function(object, x, hom_degree = NULL, ...){
  check_param(object, allow_unknown = TRUE)
  rngs <- dials::range_get(object, original = FALSE)
  if (! dials::is_unknown(rngs$upper)) {
    return(object)
  }
  
  if (! all(vapply(x, typeof, "") == "list")) {
    rlang::abort("The `pd` steps can only transform list-columns.")
  }
  
  ranges <- sapply(x, function(l) {
    v <- sapply(l, function(pd) life_support(pd, hom_degree), simplify = TRUE)
    range(v[is.finite(v)])
  })
  r <- c(max(ranges[1L,]), min(ranges[2L,]))
  len <- r[2] - r[1]
  mins <- sapply(x, function(l) {
    v <- sapply(l, function(pd) pers_min(pd, hom_degree))
    min(v[is.finite(v)])
  })
  rngs <- c(min(mins), len/2^3)
  
  dials::range_set(object, log2(sort(rngs)))
}
  
  
  
  
  
  
  
  

