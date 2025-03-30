#' @title Finalizers for persistent homology vectorizations
#'
#' @description These functions take a persistent homology vectorization
#'   parameter object and modify the [dials::unknown()] parts of `ranges` based
#'   on a data set and heuristics used in inaugural studies.
#'
#' @details
#'
#' `get_pairs_max()` sets the upper bound to the maximum number of persistent
#' pairs.
#'
#' `get_pers_max_frac()` sets both bounds to fractions of the maximum finite
#' persistence (lifespan). A single number is used as the lower bound fraction
#' and takes the upper bound fraction to be 1.
#'
#' `get_pers_min_mult()` sets both bounds to multiples of the minimum positive
#' persistence (lifespan). A single number is used as the upper bound multiple
#' and takes the lower bound multiple to be 1.
#'
#' @name vpd-finalizers
#' @inheritParams vpd-summarizers
#' @inheritParams dials::finalize
#' @param mult A double for the multiple of the data to be used for the lower
#'   bound.
#' @param ... Other arguments to pass to the underlying parameter finalizer
#'   functions.
#' @inherit dials::finalize return

#' @rdname vpd-finalizers
#' @export
get_pairs_max <- function(object, x, hom_degrees = NULL, ...) {
  check_param(object)
  
  rngs <- dials::range_get(object, original = FALSE)
  if (! dials::is_unknown(rngs$upper)) {
    return(object)
  }
  
  # check that all columns are list-columns of objects with sizes
  if (! all(vapply(x, typeof, "") == "list")) {
    rlang::abort("This step can only transform list-columns.")
  }
  
  # calculate maximum within-dimension number of pairs
  x_pairs_max <- vapply(
    x,
    \(l) max(
      vapply(l, pairs_max, 0L, hom_degrees = hom_degrees),
      na.rm = FALSE
    ),
    0L
  )
  x_max_pair <- max(x_pairs_max)
  
  # set the range based on the maximum observed and the minimum required
  rngs[2L] <- x_max_pair
  
  if (object$type == "integer" & is.null(object$trans)) {
    rngs <- as.integer(rngs)
  }
  
  dials::range_set(object, rngs)
}

#' @rdname vpd-finalizers
#' @export
get_pers_max_frac <- function(
    object, x, hom_degree = NULL, log_vals = TRUE, frac = 1/100, ...
) {
  check_param(object)
  
  rngs <- dials::range_get(object, original = FALSE)
  if (! dials::is_unknown(rngs$lower) && ! dials::is_unknown(rngs$upper)) {
    return(object)
  }
  
  frac <- as.double(frac)
  if (length(frac) == 1L) frac <- sort(c(frac, 1))
  
  # check that all columns are list-columns of objects with sizes
  if (! all(vapply(x, typeof, "") == "list")) {
    rlang::abort("This step can only transform list-columns.")
  }
  
  # calculate maximum within-dimension persistence
  x_pers_maxs <- vapply(
    x,
    function(l) {
      val <- vapply(l, pers_max, 0., hom_degree = hom_degree)
      max(val[is.finite(val)])
    },
    0.
  )
  x_pers_max <- max(setdiff(x_pers_maxs, 0))
  if (x_pers_max == -Inf) stop("No positive finite persistence values.")
  x_pers_max <- x_pers_max * frac
  
  if (dials::is_unknown(rngs$lower)) {
    rngs[1L] <- if (log_vals) log10(x_pers_max[1L]) else x_pers_max[1L]
  }
  if (dials::is_unknown(rngs$upper)) {
    rngs[2L] <- if (log_vals) log10(x_pers_max[2L]) else x_pers_max[2L]
  }
  
  if (object$type == "integer" & is.null(object$trans)) {
    rngs <- as.integer(rngs)
  }
  
  dials::range_set(object, rngs)
}

#' @rdname vpd-finalizers
#' @export
get_pers_min_mult <- function(
    object, x, hom_degree = NULL, log_vals = TRUE, mult = 100, ...
) {
  check_param(object)
  
  rngs <- dials::range_get(object, original = FALSE)
  if (! dials::is_unknown(rngs$lower) && ! dials::is_unknown(rngs$upper)) {
    return(object)
  }
  
  mult <- as.double(mult)
  if (length(mult) == 1L) mult <- sort(c(1, mult))
  
  # check that all columns are list-columns of objects with sizes
  if (! all(vapply(x, typeof, "") == "list")) {
    rlang::abort("This step can only transform list-columns.")
  }
  
  # calculate maximum within-dimension persistence
  x_pers_mins <- vapply(
    x,
    function(l) {
      val <- vapply(l, pers_min, 0., hom_degree = hom_degree)
      min(val[is.finite(val) & val > 0])
    },
    0.
  )
  x_pers_min <- min(setdiff(x_pers_mins, 0))
  if (x_pers_min == Inf) stop("No positive finite persistence values.")
  x_pers_min <- x_pers_min * mult
  
  if (dials::is_unknown(rngs$lower)) {
    rngs[1L] <- if (log_vals) log10(x_pers_min[1L]) else x_pers_min[1L]
  }
  if (dials::is_unknown(rngs$upper)) {
    rngs[2L] <- if (log_vals) log10(x_pers_min[2L]) else x_pers_min[2L]
  }
  
  if (object$type == "integer" & is.null(object$trans)) {
    rngs <- as.integer(rngs)
  }
  
  dials::range_set(object, rngs)
}
